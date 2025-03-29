
############################################################################################
# build_all.sh - build the scalation project for updated jar file

echo ================================= scalation ===========================================
echo // "sbt clean compile package"
         sbt clean compile package

echo // "find . -name *.jar"
         find . -name *.jar

