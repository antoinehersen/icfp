pushd src
make clean
popd
tar -cf ahersen.tar src install
gzip ahersen.tar
mv ahersen.tar.gz submission/ahersen-$(date "+%Y-%m-%d_%H%M%S").tar.gz
