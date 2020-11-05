#!/usr/bin/env bash
set -ex

wget -q -O - "https://repos.ripple.com/repos/api/gpg/key/public" | apt-key add -
# check ripple repo fingerprint
apt-key finger | grep "C001 0EC2 05B3 5A33 10DC  90DE 395F 97FF CCAF D9A"

echo "deb https://repos.ripple.com/repos/rippled-deb xenial stable" | tee -a /etc/apt/sources.list.d/ripple.list

apt-get -y update
apt-get -y install rippled

# Setup mock parity server
curl -O https://releases.parity.io/ethereum/v2.5.9/x86_64-unknown-linux-gnu/parity
cp $(pwd)/parity /usr/local/bin/
chmod 755 /usr/local/bin/parity