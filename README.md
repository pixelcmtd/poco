# poco

Generate compose files from portable mappings.

`poco` takes a simple YAML mapping of trigger sequences to replacement text and
turns it into a native compose file for a target platform. It is written in
Haskell and is currently a work in progress.

## Usage

```
poco [-t|--target TARGET] <input.yaml>
```

Example input file (see also: `example.yaml`):

```yaml
"ae": "ä"
"oe": "ö"
"ue": "ü"
"hug": "🫂"
"..": "…"
```

Run `poco` on the bundled example and compare with the expected output:

```
stack build
stack exec poco-exe -- example.yaml
diff example.dict <(stack exec poco-exe -- example.yaml)
```

### Targets

| `TARGET` | Output                                                                                                                                                                                  | Status      |
| -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------- |
| `Cocoa`  | [`DefaultKeyBinding.dict`](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/EventOverview/TextDefaultsBindings/TextDefaultsBindings.html) for macOS (default) | implemented |
| `XComp`  | [`XCompose`](https://manned.org/man.3c6fbf56/arch/Compose.5) file for X11                                                                                                               | not yet     |

Cocoa output deliberately mimics the Python tool
[`gen-compose`](https://github.com/Granitosaurus/macos-compose). For example,
`"hug": "🫂"` becomes:

```
{"§" = {
  "h" = {
    "u" = {
      "g" = ("insertText:", "🫂");
    };
  };
};}
```

`example.dict` shows the full `gen-compose` output for `example.yaml`.
It currently differs from `poco -t Cocoa` output in sorting.

## Notes

- The current implementation assumes sequence is a prefix of another; but the
  code doesn't test for this yet.
- The `XComp` target is stubbed out and not yet implemented.
