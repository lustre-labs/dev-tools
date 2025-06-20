{
  description = "Gleam Web Development";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=c2a03962b8e24e669fb37b7df10e7c79531ff1a4";
    flake-utils.url = "github:numtide/flake-utils?rev=11707dc2f618dd54ca8739b309ec4fc024de578b";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            # erlang
            pkgs.beam27Packages.erlang
            pkgs.beam27Packages.rebar3

            # gleam
            pkgs.gleam

            # tailwind
            pkgs.tailwindcss_4
          ];
          shellHook = "";
        };
      }
    );
}
