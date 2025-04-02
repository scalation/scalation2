
####################################################################################################
# build_all.sh - build the scalation project and copy its jar file to the lib directory

echo ================================ scalation =========================================
echo // "sbt compile package"
sbt compile package

echo // "cp target/scala-3.0.1/scalation_3-0.1.0-SNAPSHOT.jar lib"
cp target/scala-3.0.1/scalation_3-0.1.0-SNAPSHOT.jar lib

