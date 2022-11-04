{
  description = "Spaceship";
  inputs.hix.url = "github:tek/hix";
  outputs = {hix, ...}:
    hix.lib.flake ({config, ...}: {
      base = ./.;
      packages = {hello = ./.;};
      ghcid.shellConfig = {
        buildInputs = with config.pkgs; [
          haskellPackages.hindent
          treefmt
          alejandra
        ];
      };
    });
}
