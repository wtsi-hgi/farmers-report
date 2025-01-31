terraform {
  required_providers {
    openstack = {
      source = "terraform-provider-openstack/openstack"
      version = "2.1.0"
    }
    infoblox = {
      source = "infobloxopen/infoblox"
      version = "2.7.0"
    }
  }

  backend "s3" {
    bucket = "terraform-remote-state"
    workspace_key_prefix = "farmers-report-app"
    key    = "farmers-report-app/terraform.tfstate"
    region = "us-east-1"
    skip_credentials_validation = true
    skip_requesting_account_id  = true
    skip_s3_checksum            = true
  }
}

provider "infoblox" {
  server   = var.infoblox_host
  username = var.infoblox_user
  password = var.infoblox_pass
}

variable "public_key" {
  type        = string
  description = "Path to public key to be used in server"
  default     = "~/.ssh/id_rsa.pub"
}

variable "nfs_share" {
  type        = string
  description = "Path to NFS share for SMB mount"
  nullable    = false
}

variable "smbcredentials" {
  type        = string
  description = "Path to a file with credentials for SMB mount"
  nullable    = false
}

variable "farm_config" {
  type        = string
  description = "Path to farmers report config file"
  default     = "./config.yaml"
}

locals {
  farmers_config = yamldecode(file(var.farm_config))
}

variable "infoblox_user" {
  type        = string
  description = "username for infoblox"
  nullable    = false
}

variable "infoblox_pass" {
  type        = string
  description = "password for infoblox"
  nullable    = false
}

variable "infoblox_host" {
  type        = string
  description = "infoblox server address"
  nullable    = false
}

resource "openstack_compute_keypair_v2" "kp" {
  name       = "shinyproxy-keypair"
  public_key = file(var.public_key)
}

data "openstack_networking_network_v2" "external" {
  external = true
}

resource "openstack_networking_network_v2" "network" {
  name           = "shinyproxy-network"
}

resource "openstack_networking_subnet_v2" "subnet" {
  name           = "shinyproxy-subnetwork"
  network_id     = openstack_networking_network_v2.network.id
  cidr           = "192.168.0.0/24"
}

resource "openstack_networking_floatingip_v2" "floating_ip" {
  description = "shinyproxy public IP"
  pool        = data.openstack_networking_network_v2.external.name
  port_id     = openstack_networking_port_v2.port.id
}

resource "openstack_networking_port_v2" "port" {
  name       = "shinyproxy-port"
  network_id = openstack_networking_network_v2.network.id
  fixed_ip {
    subnet_id = openstack_networking_subnet_v2.subnet.id
  }
}

resource "openstack_networking_router_v2" "router" {
  name                = "shinyproxy-router"
  external_network_id = data.openstack_networking_network_v2.external.id
}

resource "openstack_networking_router_interface_v2" "router_interface" {
  router_id = openstack_networking_router_v2.router.id
  subnet_id = openstack_networking_subnet_v2.subnet.id
}

resource "openstack_networking_secgroup_v2" "secgroup" {
  name                 = "shinyproxy-secgroup"
  description          = "Security group for shinyproxy"
  delete_default_rules = true
}

resource "openstack_networking_secgroup_rule_v2" "shinyproxy_web_port" {
  direction         = "ingress"
  ethertype         = "IPv4"
  protocol          = "tcp"
  port_range_min    = 8080
  port_range_max    = 8080
  security_group_id = openstack_networking_secgroup_v2.secgroup.id
}

resource "openstack_compute_instance_v2" "server" {
  name            = "shinyproxy-server"
  image_name      = "jammy-WTSI-docker_324910_82eec972"
  flavor_name     = "m2.xlarge"
  key_pair        = openstack_compute_keypair_v2.kp.name
  security_groups = [
    "default",
    "cloudforms_ssh_in",
    openstack_networking_secgroup_v2.secgroup.name
  ]

  network {
    port          = openstack_networking_port_v2.port.id
  }

  user_data       = templatefile("startup.yaml", {
    farm_config       = filebase64(var.farm_config)
    shinyproxy_config = filebase64("./shinyproxy.yml")
    smbcredentials    = filebase64(var.smbcredentials)
    nfs_share         = var.nfs_share
  })
}

output "instance_ip_addr" {
  value = openstack_networking_floatingip_v2.floating_ip.address
}

resource "infoblox_a_record" "dns_record" {
  fqdn     = "farmer${terraform.workspace == "default" ? "" : "-dev"}.hgi.sanger.ac.uk"
  dns_view = "internal"
  ip_addr  = openstack_networking_floatingip_v2.floating_ip.address
}
