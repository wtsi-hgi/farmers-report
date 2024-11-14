# Farmers report

## Usage
To use locally you need to prepare a `config.yaml` based on the example below:

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
Code contains instructions to create an OpenStack VM and configure it as follows.

Terraform creates all cloud resources and do not reuse any existing objects. It creates
* keypair
* network and subnetwork
* external floating IP
* security group with open 8080 port
* VM
* DNS record

OpenStack instance is configured using `cloud-config` defined in `terraform/startup.yaml`. It
* mounts NFS (`nfs.mount` systemd service)
* writes config files (for SMB mount, go-farmer, farm-dashboard)
* installs cifs-utils, go, go-farmer, shinyproxy
* runs go-farmer (`go-farmer.service` systemd service)
* writes shinyproxy configuration (`terraform/shinyproxy.yml`)

#### Initialisation (needed only once) 

To initialise terraform prepare a config file `config.s3.tfbackend` with your s3 credentials:
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
Now download `openrc.sh` file for your OpenStack tenant.

#### Deploy

Prepare a config file `config.yaml` based on `config-template.yaml`.
Now prepare a file `terraform/terraform.tfvars` based on `terraform-template.tfvars` where `farm_config` would point to your `config.yaml`. 
Now execute

```bash
cd ./terraform
source openrc.sh
terraform apply
```

Terraform will now update infrastructure according to `main.tf`.

You can manage different deployments by changing terraform workspaces. 
You have to use different OpenStack tenants for each deployment.

### Update application on the server

Build and push to Docker Hub a new image.
Go to the shinyproxy web-page and launch the app. 
Shinyproxy will spawn a new container in the background from the new image.
Close the app.
The old container will die in a minute.
