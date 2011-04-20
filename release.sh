set -e
rm -f *.jar pom.xml
lein test
lein jar
lein pom
scp pom.xml impresario*.jar clojars@clojars.org:
