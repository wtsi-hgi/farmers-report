# Farmers report

## Usage
To use locally, you need to prepare a `config.yaml` based on the example below:

```yaml
proxy:
  host: "host-of-go-farmer.com"
  port: 12321
  scheme: "http"
  username: "user"
  password: "pass"
  index: "elastic-index"
```

Then, to use within Sanger, just load SoftPack environment and start RStudio
```bash
module load HGI/softpack/groups/hgi/farmers-report
module load HGI/common/rstudio
rstudio start -M 8000
```

You can now knit a report or serve an app.

## Development
To run tests simply execute within an environment
```bash
Rscript tests/testthat.R
```

To calculate the coverage
```bash
Rscript tests/covr.R
```
The coverage reports can be found in reports/ directory.

To build a Docker image
```bash
docker build -t mercury/farmers-report:latest .
```

To run tests inside a container
```bash
docker run --rm -v $(pwd):/code -w /code mercury/farmers-report:latest Rscript /code/tests/testthat.R
```

## Deployment

### Infrastructure 

Infrastructure is managed via Terraform with a remote backend in a s3 bucket to enable shared access to Terraform state file.
Code contains instructions just to create an OpenStack VM as follows. Configuration is done later.

Terraform creates all cloud resources and does not reuse any existing objects. It creates
* keypair
* network and subnetwork
* external floating IP
* security group with open 8080 port
* VM
* DNS record

#### Initialisation (needed only once) 

To initialise terraform, prepare a config file `config.s3.tfbackend` with your s3 credentials:
```terraform
access_key = "your-access-key"
secret_key = "your-secret-key"
endpoints  = {
        s3 = "https://your-host-base"
}
```

Ensure your user has access to a s3 bucket and execute
```bash
terraform init -backend-config="config.s3.tfbackend"
```

#### Deploy

Now download `openrc.sh` file for your OpenStack tenant.

Prepare a file `terraform/terraform.tfvars` based on `terraform-template.tfvars`. 
Now execute

```bash
source openrc.sh
cd ./terraform
export AWS_REQUEST_CHECKSUM_CALCULATION=when_required
terraform apply
```

Terraform will now update infrastructure according to `terraform/main.tf`.

You can manage different deployments by changing terraform workspaces:
```bash
terraform workspace list
terraform workspace select dev
terraform workspace select default
```

You have to use different OpenStack tenants (read different `openrc.sh` files) for each deployment.

### Configuration
Configuration is managed via Ansible.

OpenStack instance is configured using playbook defined in `ansible/playbook.yml`. It
* mounts NFS (`nfs.mount` systemd service)
* installs go-farmer (`go-farmer.service` systemd service)
* installs shinyproxy
* installs and configures GitHub actions runner
* writes shinyproxy configuration (`ansible/roles/shinyproxy/files/shinyproxy.yml`)
* writes other config files (for SMB mount, go-farmer, farm-dashboard)

Prepare a config file `config.yaml` based on `config-template.yaml`.  
Next, prepare `ansible/vars.yml` file based on `ansible/vars-template.yml` where `farmers_config` would point to your `config.yaml`.

Now execute:
```bash
ansible-galaxy install -r ansible/requirements.yml -p ansible/roles/
ansible-playbook -i ansible/inventory/prod.ini -u ubuntu --private-key /path/to/key ansible/playbook.yml -e "@ansible/vars.yml"
```
Use the corresponding private key to the one that you used in `terraform.tfvars` file.  
NB use `ansible/inventory/dev.ini` to configure a dev instance.

### Update application on the server

Build and push to Docker Hub a new image.
Go to the shinyproxy web-page and launch the app. 
Shinyproxy will spawn a new container in the background from the new image.
Close the app.
The old container will die in a minute.
