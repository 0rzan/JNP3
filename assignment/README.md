# PostKrypt
note: no support for reading from standard input (stdin).

### Requirements
graphics 5.1.2

### Build
```sh
cd PostKrypt
dune build
```

### Basic Usage
- Graphic Output:
  ```sh
  dune exec PostKrypt -- -n 2 -d path/to/input
  ```
  Use the `-d` flag to generate graphic output.
  Use the `-n` flag to set the magnification factor (default is 1).

- Standard Output:
  ```sh
  dune exec PostKrypt -- path/to/input
  ```

To display using `gv`:
```sh
dune exec PostKrypt -- path/to/input > path/to/output
gv path/to/output
```
