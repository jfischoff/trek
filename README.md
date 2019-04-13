# trek

`trek` is a database migration system. Unlike most database migration systems `trek` is designed to handle development and QA workflows which could require the same migration is applied twice after a modification. The `trek` production workflow is very similar to other popular migration systems like `Active Migration`.

The three workflows, `dev`, `qa` and `prod` are described below.

Currently on PostreSQL is supported.

### `dev` Workflow

The dev workflow is built for a single developer that uses a test database which can be discarded and recovered. If an already applied migration has been modified `trek` will dump the db and recreate it from scratch.

### `qa` Workflow

The `qa` workflow is a little more intelligent than the `dev` workflow. It is meant for a persistent QA environment that is backed up and has complex data one would not like to lose. If an already applied migration changes `trek` will find the backup that was taken before the migration was applied and restore to that point and then roll forward. In this way the data lost is much less than in the `dev` workflow but one needs to have a db backup strategy in place which is overkill for `dev`.

### `prod` Workflow

The `prod` workflow lacks the ability to check if a migration has been modified. It can only apply unapplied migrations.

## Reducing Drift

One of the goals of `trek` is to help allow developers to test against what is in production. To help reduce drift between what is in production and the database schema one would obtain by applying all of the migrations in sequence, `trek` can utilize schema dumps of production.

`trek` supports this optional workflow and provides convience functions for restore QA using a schema dump so that the two persistent environments (QA and production) do not drift from each other.

This is feature `trek` shares with other migration systems as well.

### Other Features Common in Migration Systems
- Dry run with detailed statistics (tables modified and estimate size of the change)
- Transaction control (single, multiple, etc)
- Metrics integration (Timing, counts, etc)
- Detailed logging
- Rich command line
- Audit log of all migrations

### Other Features Missing
- Seeding e.g. data loading



