# ocaml-profiles
Stores opam configurations on branches or tags for bootstrapping ocaml environments.

## Installation

```
git clone https://github.co/struktured/ocaml-profiles -b stable
./pin.sh
```

# Usage

## Overview
Profiles are are stored by default at whatever the ```OCAML_PROFILES_URL``` environment resolves to (defaults to this repository).
Any git branch starting with "profile/" is a possible profile. Run with no arguments or "--help" 
to get a full help menu.

## Applying a profile

```ocaml-profiles my_prof```

will install profile ```my_prof``` by fetching "profiles/my_prof" github branch and scanning the profile configurations, 
applying any opam pins and installs as needed.

## Listing available profiles


## Creating new profi;es.



