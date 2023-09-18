with (import <nixpkgs> {});
mkShell {
    nativeBuildInputs = [
        stack
        wasmtime
        wabt
    ];
}
