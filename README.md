# ocaml-profiles
Stores opam configurations on branches or tags for bootstrapping ocaml environments.

## Installation

```
git clone https://github.com/struktured/ocaml-profiles -b stable
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

Any of the following will list all profiles on the default repository:

```
ocaml-profiles -list 
ocaml-profiles list
ocaml-profiles -l
ocaml-profiles
```

## Showing an existing profile

```
ocaml-profiles show struktured

Cloning into '.ocaml-profiles/struktured'...
remote: Counting objects: 20, done.
remote: Compressing objects: 100% (3/3), done.
remote: Total 20 (delta 0), reused 0 (delta 0), pack-reused 17
Unpacking objects: 100% (20/20), done.
Checking connectivity... done.
Profile "struktured":
 profiles:
        async
        developer-extras
        ocamlscript
        shell-support
        riakc_ppx
        prob-cache
 pins:

 packages:
        csv re extlib fileutils cmdliner calendar ocamlscript prob-cache csv riakc_ppx

```


## Creating new profiles

To create a new profile, simply clone any profile from ```profiles/*``` and modify as needed.

Add any customized pins to ```pinned```, any packages to install to ```packages``` and any profiles
which should be invoked prior to this profile in ```profiles```. 


