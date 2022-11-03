{
  description = "Spaceship";
  inputs.hix.url = "github:tek/hix";
  outputs = {hix, ...}:
    hix.lib.flake ({config, ...}: {
      base = ./.;
      packages = {hello = ./.;};
      ghcid.shellConfig = {
        buildInputs = [config.pkgs.haskellPackages.hindent];
      };
    });
}
