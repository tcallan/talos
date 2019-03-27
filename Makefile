PACKAGE_DIR=../..

build:
	dotnet build src/

clean:
	dotnet clean src/

test:
	dotnet test src/

pack-talos:
	dotnet pack -o $(PACKAGE_DIR) src/Talos

pack-dynamic:
	dotnet pack -o $(PACKAGE_DIR) src/Talos.Dynamic

pack: pack-talos pack-dynamic

run-example:
	dotnet run --project src/Talos.Dynamic.Example