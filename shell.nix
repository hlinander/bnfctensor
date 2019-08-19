with import <nixpkgs> {};

let
  hie-nix = fetchFromGitHub {
      owner = "domenkozar";
      repo = "hie-nix";
      rev = "6794005f909600679d0b7894d0e7140985920775";
      sha256 = "0pc90ns0xcsa6b630d8kkq5zg8yzszbgd7qmnylkqpa0l58zvnpn";
    };
  hiepkgs = import hie-nix {};
in
stdenv.mkDerivation {
	name = "ghc";
	buildInputs = [(haskellPackages.ghcWithPackages (pkgs: [pkgs.hlint pkgs.hindent pkgs.combinat])) readline pkg-config vscode cargo rustc rls rustPlatform.rustcSrc gdb lldb];
	RUST_SRC_PATH = "${rustPlatform.rustcSrc}";
	#RUST_SRC_PATH = "${rustPlatform.rustcSrc}/lib/rustlib/src/rust/src";
}
