[![Travis CI Status](https://travis-ci.org/jfischoff/trek.svg?branch=master)](http://travis-ci.org/jfischoff/trek)
# Aspirational README. WIP.

`trek` is a simple database migrator. It has two commands: `create` and `apply`.

## `trek create`

### Example
```bash
$ trek create migration
migration-2020-07-09T06:21:12.667796Z
```
### Description

#### Successful Behavior

`create NAME` will make a `NAME-VERSION.sql` file in the path pointed at by `NAME` using the current date time as ISO8601 `VERSION`.

If the file exists `create` will retry **one time only** with a version incremented by one second.

#### Errors

`create` will return a exit code of `1` if the file creation fails for any reason.

## `trek apply`

### Example
```bash
$ trek apply DIRPATH
{ "hash" : "503dc144019a6d61db66b854c6e01127b94f644d"
, "created_at" : "2020-01-01T22:31:04"
, "migrations" :
    [ { "name"    : "foo"
      , "version" : "2019-01-01T22:31:04"
      , "hash"    : "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
      }
    , { "name"    : "bar"
      , "version" : "2019-02-01T22:31:04"
      , "hash"    : "62cdb7020ff920e5aa642c3d4066950dd1f01f4d"
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
