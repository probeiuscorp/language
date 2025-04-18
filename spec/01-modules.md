# Modules
The module system is mostly inspired by ECMAScript's.

I dislike Haskell's module system because
- I have to pick a project name, and when I inevitably change it I have to rewrite all my imports
- I have to scroll to the top of the file to export something, which means
- My git diffs are split up and uglier

Overall, it feels heavy on ceremony.

I mostly like ECMAScript's modules, except for
- default exports/imports
- placing the module specifier after the import list

## Exporting
Add `export` modifier before declarations:
```
export main = putStrLn "Hello, world"
export type Pair = a. (a, a)
```

If type and body are separated, `export` the type:
```
export main: IO ()
main = putStrLn "Hello, world"
```

Bindings can be (re-)exported:
```
export { main, Pair }
```

Export lists can rename:
```
putHelloWorld = putStrLn "Hello, world"
export {
  main = putHelloWorld
}
```

## Importing

Import a specifier to put all of the module's exported bindings in scope.

<table>
<tr>
<td>

`importer.tl`

</td>
<td>

`some-file.tl`

</td>
</tr>
<tr>
<td>

```
import ./some-file                       
export { main }
```

</td>
<td>

```
export main = putStrLn "Hello, world"    
export type Pair = a. (a, a)
```

</td>
</tr>
</table>

Add import list to whitelist.
```
import ./some-file { main }
```

Add `hiding` to blacklist.
```
import ./some-file hiding { main }
```

Add `as` to put all imports in record.
```
import ./some-file as SomeFile
export { main = SomeFile.main }
```
