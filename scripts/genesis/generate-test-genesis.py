#!/usr/bin/env python3
import json
import sys
import os
import subprocess
import shutil

# The docker runner is designed to work with the image built by the
# ../../concordium-base/scripts/genesis-tools.Dockerfile script.
class DockerRunner:

    # NB: This needs the docker library
    # pip3 install docker

    def __init__(self):
        self.image = os.environ.get("GENESIS_TOOLS_DOCKER_IMAGE", default = "concordium/genesis-tools:0.5")
        import docker
        self.client = docker.from_env()
        self.root = os.getcwd()

    def run_binary(self, name, *args):
        import docker
        from docker.errors import ContainerError
        try:
            res = self.client.containers.run(self.image, [name] + list(args),
                                                working_dir="/home/workspace",
                                                volumes={self.root: {"bind": "/home/workspace", "mode": "rw"}},
                                                stdout=True,
                                                stderr=True,
                                                stream=True,
                                             )
            for out in res:
                print(out.decode(),end='')
            return 0
        except ContainerError as exc:
            print(exc.stderr.decode())
            return 1

    def run_generate_keys(self, *args):
        return self.run_binary("generate-update-keys", *args)

    def run_genesis(self, *args):
        return self.run_binary("genesis", *args)

    def run_client(self, *args):
        return self.run_binary("client", *args)

    def run_genesis_tool(self, *args):
        return self.run_binary("genesis_tool", *args)

# Runner that uses native binaries
class HostRunner:

    def run_generate_keys(self, *args):
        res = subprocess.run(GENERATE_UPDATE_KEYS + list(args))
        return res.returncode

    def run_genesis(self, *args):
        res = subprocess.run(GENESIS_DAT_TOOL + list(args))
        return res.returncode

    def run_client(self, *args):
        res = subprocess.run(CLIENT_TOOL + list(args))
        return res.returncode

    def run_genesis_tool(self, *args):
        res = subprocess.run(GENESIS_ACCOUNTS_TOOL + list(args))
        return res.returncode


if os.environ.get("USE_DOCKER") is not None:
    runner = DockerRunner()
else:
    GENESIS_DAT_TOOL=os.environ.get("GENESIS_DAT_TOOL", default = "./genesis").split()

    # The tool to generate identity providers, parameters, and anonymity revokers
    CLIENT_TOOL=os.environ.get("CLIENT_TOOL", default = "./client").split()

    # The tool to create genesis accounts
    GENESIS_ACCOUNTS_TOOL=os.environ.get("GENESIS_ACCOUNTS_TOOL", default = "./genesis_tool").split()

    GENERATE_UPDATE_KEYS = os.environ.get("GENERATE_UPDATE_KEYS", default = "./generate-update-keys").split()

    runner = HostRunner()

# If used with docker genesis tools image GENESIS_DIR must be a path under the current working directory.
GENESIS_DIR = os.environ.get("GENESIS_DIR", default = "./genesis_data")

GENESIS_STRING = os.environ.get("GENESIS_STRING", default = "Genesis description")

# The file with genesis parameters.
# It is the intention that this will be modified manually depending on what configuration is desired.
# The only change we make in this file is to update the genesis account after we have generated genesis accounts.
GENESIS_PARAMETERS_FILE = os.environ.get("GENESIS_FILE", default = "genesis.json")

# Number of identity providers that will be generated.
NUM_IPS = os.environ.get("NUM_IPS", default = "3")
# Number of anonymity revokers that will be generated.
NUM_ARS = os.environ.get("NUM_ARS", default = "3")

# Initial balance of all the accounts that will be created.
INITIAL_BALANCE = os.environ.get("INITIAL_BALANCE", default = "3500000.0")

# Initial stake of all the bakers that will be created.
INITIAL_STAKE = os.environ.get("INITIAL_STAKE", default = "3000000.0")

# The number of bakers that will be created.
NUM_BAKERS = os.environ.get("NUM_BAKERS", default = "5")
# The number of account keys on each of the generated accounts
NUM_KEYS = os.environ.get("NUM_KEYS", default = "1")

MAX_BLOCK_ENERGY = os.environ.get("MAX_BLOCK_ENERGY", default = "3000000")

# Balance of the foundation account
FOUNDATION_ACCOUNT_BALANCE=os.environ.get("FOUNDATION_ACCOUNT_BALANCE", "1000000")

# Template for any extra accounts.
EXTRA_ACCOUNTS_TEMPLATE=os.environ.get("EXTRA_ACCOUNTS_TEMPLATE")
# Number of extra accounts to generate, only used if EXTRA_ACCOUNTS_TEMPLATE is provided.
NUM_EXTRA_ACCOUNTS=os.environ.get("NUM_EXTRA_ACCOUNTS")
# Balance of additional accounts, only used if EXTRA_ACCOUNTS_TEMPLATE is provided.
EXTRA_ACCOUNTS_BALANCE=os.environ.get("EXTRA_ACCOUNTS_BALANCE")

# Helper defined constants
GLOBAL_FILE = os.path.join(GENESIS_DIR, "global.json")

# Create cryptographic parameters, identity providers, and anonymity revokers
def create_base_parameters():
    if os.path.exists(GENESIS_DIR):
        raise Exception(f"Refusing to overwrite genesis directory {GENESIS_DIR}")
    else:
        os.makedirs(GENESIS_DIR)
    res = runner.run_client("generate-global", "--string", GENESIS_STRING, "--out-file", GLOBAL_FILE)
    if res != 0:
        raise Exception(f"Error creating global parameters {res.stderr} {res.stdout}")

    ips = runner.run_client("generate-ips", "--global", GLOBAL_FILE, "--num", NUM_IPS, "--num-ars", NUM_ARS, "--out-dir", GENESIS_DIR)
    if ips != 0:
        raise Exception(f"Could not create identity providers {res.stderr}")

def create_bakers():
    bakers_dir = os.path.join(GENESIS_DIR, "bakers")
    if os.path.exists(bakers_dir):
        raise Exception(f"Refusing to overwrite directory {bakers_dir}")
    else:
        os.makedirs(bakers_dir)

    res = runner.run_genesis_tool("create-accounts", "--ar-info", os.path.join(GENESIS_DIR, "AR-1.pub.json"),
                          "--ip-info", os.path.join(GENESIS_DIR, "identity_provider-0.pub.json"),
                          "--global", GLOBAL_FILE,
                          "--num", NUM_BAKERS,
                          "--num-keys", NUM_KEYS,
                          "--stake", INITIAL_STAKE,
                          "--balance", INITIAL_BALANCE,
                          "--template", "baker-account",
                          "--out-dir", bakers_dir)
    if res != 0:
        raise Exception(f"Could not create baker accounts {res.stderr}")


# Create some extra accounts
def create_accounts(template = "foundation-account", num = "1", balance = FOUNDATION_ACCOUNT_BALANCE):
    accounts_dir = os.path.join(GENESIS_DIR, f"{template}s")
    if os.path.exists(accounts_dir):
        raise Exception(f"Refusing to overwrite directory {accounts_dir}")
    else:
        os.makedirs(accounts_dir)

    res = runner.run_genesis_tool("create-accounts", "--ar-info", os.path.join(GENESIS_DIR, "AR-1.pub.json"),
                          "--ip-info", os.path.join(GENESIS_DIR, "identity_provider-0.pub.json"),
                          "--global", GLOBAL_FILE,
                          "--num", num,
                          "--balance", balance,
                          "--template", template,
                          "--out-dir", accounts_dir)
    if res != 0:
        raise Exception(f"Could not create foundation accounts {res.stderr}")
    if template == "foundation-account": # return the address of the foundation account.
        with open(os.path.join(accounts_dir, "foundation-account-0.json"), "r") as f:
            data = json.load(f)
            return data["address"]


def generate_update_keys():
    updates_dir = os.path.join(GENESIS_DIR, "updates")
    if os.path.exists(updates_dir):
        raise Exception(f"Refusing to overwrite directory {updates_dir}")
    else:
        os.makedirs(updates_dir)
    res = runner.run_generate_keys(
                          "47",
                          "--keys-outfile", os.path.join(updates_dir, "authorizations.json"),
                          "--keys-outdir", updates_dir,
                          "--root-keys", "3:5",
                          "--level1-keys", "3:5",
                          "--emergency", "3:0,1,2,3,4",
                          "--protocol", "2:5,6,7",
                          "--election", "2:8,9,10",
                          "--euro-energy", "2:8,9,10",
                          "--gtu-euro", "1:11",
                          "--foundation-account", "3:12,13,14,15,16",
                          "--mint-distribution", "3:17,18,19,20,21",
                          "--fee-distribution", "3:22,23,24,25,26",
                          "--gas-rewards", "3:27,28,29,30,31",
                          "--baker-minimum-threshold", "3:32,33,34,35,36",
                          "--add-anonymity-revoker", "3:37,38,39,40,41",
                          "--add-identity-provider", "3:42,43,44,45,46"
                          )
    if res != 0:
        raise Exception(f"Could not generate update keys.")


# Combine all the accounts into a single accounts file to be fed into the genesis tool.
def combine(foundation_account, extra = None):
    ips_path = os.path.join(GENESIS_DIR, "identity_providers.json")
    ars_path = os.path.join(GENESIS_DIR, "anonymity_revokers.json")

    authorizations = os.path.join(GENESIS_DIR, "updates", "authorizations.json")

    with open(GENESIS_PARAMETERS_FILE, "r") as gfile:
        data = json.load(gfile)
        data["value"]["chainParameters"]["foundationAccount"] = foundation_account
        data["value"]["maxBlockEnergy"] = int(MAX_BLOCK_ENERGY)
        with open("genesis-tmp.json", "w") as out_file:
            json.dump(data, out_file)

    accounts = os.path.join(GENESIS_DIR, "initial_accounts.json")

    with open(accounts, "w") as out_file:
        with open(os.path.join(GENESIS_DIR, "bakers", "baker-accounts.json")) as ba:
            with open(os.path.join(GENESIS_DIR, "foundation-accounts", "foundation-accounts.json")) as fa:
                bas = json.load(ba)
                fas = json.load(fa)
                if extra is not None:
                    with open(os.path.join(GENESIS_DIR, f"{extra}s", f"{extra}s.json")) as ea:
                        eas = json.load(ea)
                        json.dump(bas + fas + eas, out_file)
                else:
                    json.dump(bas + fas, out_file)

    out_genesis = os.path.join(GENESIS_DIR, "genesis.dat")

    res = runner.run_genesis(
                          "make-genesis",
                          f"--identity-providers={ips_path}",
                          f"--anonymity-revokers={ars_path}",
                          f"--crypto-params={GLOBAL_FILE}",
                          f"--accounts={accounts}",
                          f"--update-keys={authorizations}",
                          "genesis-tmp.json",
                          out_genesis)
    if res != 0:
        raise Exception("Could not generate genesis.dat")


# If the PURGE environment variable is set then delete the genesis directory before starting anything else
def clean():
    if os.environ.get("PURGE") is not None:
        if os.path.exists(GENESIS_DIR):
            shutil.rmtree(GENESIS_DIR)
        else:
            print(f"PURGE set, but no {GENESIS_DIR} does not exist, so nothing was deleted.")


clean()
# Create identity providers, anonymity revokers, and cryptographic parameters
create_base_parameters()
# Create all the bakers
create_bakers()
# Create a single foundation account, and return its address
foundation_account = create_accounts()
if EXTRA_ACCOUNTS_TEMPLATE is not None:
    create_accounts(template = EXTRA_ACCOUNTS_TEMPLATE, num = NUM_EXTRA_ACCOUNTS, balance = EXTRA_ACCOUNTS_BALANCE)
# Generate keys for all the updates.
generate_update_keys()
# And finally combine everything and output genesis.dat suitable for starting a baker.
combine(foundation_account, extra = EXTRA_ACCOUNTS_TEMPLATE)
