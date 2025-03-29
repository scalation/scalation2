
############################################################################################
# releae.sh - build the scalation project for release

echo ================================= scalation ===========================================
echo // "sbt clean compile doc package; sbt runMain scalation.genIndexHtml"
         sbt clean compile doc package; sbt "runMain scalation.genIndexHtml"

echo // "find . -name *.jar"
         find . -name *.jar

