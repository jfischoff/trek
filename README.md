# Aspirational README. WIP. Does **not** compile.

`trek` is a simple database migrator. It has two commands: `create` and `apply`.

## `trek create`

### Example
```bash
$ trek create NAME
```
### Description

#### Successful Behavior

`create NAME` will make a `NAME-VERSION.sql` file in the path pointed at by `NAME` using the current date time as ISO8601 `VERSION`.

#### Errors

`create` will return a exit code of `1` if the file creation fails for any reason.

## `trek apply`

### Example
```bash
$ trek apply DIRPATH
{ "created_at" : "2020-01-01T22:31:04"
, "migrations" :
    [ { "name"    : "foo"
      , "version" : "2019-01-01T22:31:04"
      , "hash"    : "xofdshagnosfdasngs"
      }
    , { "name"    : "bar"
      , "version" : "2019-02-01T22:31:04"
      , "hash"    : "barbar"
      }
    ]
}
```

### Description

#### Successful Behavior

`apply` executes the the non-applied migrations (`*.sql` files) in `DIRPATH` together in a single `SERIALIZABLE` transaction.

`apply` returns a JSON list of migrations entries as seen in the example above. *The formatting of `trek` JSON is not shown*.

#### Errors

- If any of the sql files fail the transaction will be aborted and `trek` will return an exit code of `1`.
- if `DIRPATH` contains zero `*.sql` files `apply` returns a exit code of `2`.
- if any `*.sql` files cannot be parsed in the `NAME-VERSION.sql` format `apply` will return exit code `3`.

#### Environment Variables

You can configure `trek` by setting the standard postgres connection [parameters](https://www.postgresql.org/docs/current/libpq-envars.html).
