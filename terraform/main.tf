terraform {
  required_providers {
    openstack = {
      source = "terraform-provider-openstack/openstack"
      version = "2.1.0"
    }
  }
}

data "openstack_compute_keypair_v2" "kp" {
  name = "ip13-key"
}

data "openstack_networking_network_v2" "external" {
  external = true
}

resource "openstack_networking_network_v2" "network" {
  name           = "shinyproxy_network"
}

resource "openstack_networking_subnet_v2" "subnet" {
  name           = "shinyproxy_subnetwork"
  network_id     = openstack_networking_network_v2.network.id
  cidr           = "192.168.0.0/24"
}

resource "openstack_networking_floatingip_v2" "floating_ip" {
  description = "shinyproxy public IP"
  pool        = data.openstack_networking_network_v2.external.name
  port_id     = openstack_networking_port_v2.port.id
}

resource "openstack_networking_port_v2" "port" {
  name       = "shinyproxy_port"
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

resource "openstack_networking_secgroup_rule_v2" "secgroup_rule_8080" {
  direction         = "ingress"
  ethertype         = "IPv4"
  protocol          = "tcp"
  port_range_min    = 8080
  port_range_max    = 8080
  security_group_id = openstack_networking_secgroup_v2.secgroup.id
}

resource "openstack_compute_instance_v2" "server" {
  name            = "shinyproxy_server"
  image_name      = "jammy-WTSI-docker_247771_4ea57c30"
  flavor_name     = "m4.small"
  key_pair        = data.openstack_compute_keypair_v2.kp.name
  security_groups = [
    "default",
    "cloudforms_ssh_in",
    openstack_networking_secgroup_v2.secgroup.name
  ]

  network {
    port          = openstack_networking_port_v2.port.id
  }

  user_data       = templatefile("startup.yaml", {
    farm_config       = filebase64("../config-elastic.yaml")
    shinyproxy_config = filebase64("./shinyproxy.yml")
  })
}

output "instance_ip_addr" {
  value = openstack_networking_floatingip_v2.floating_ip
}
