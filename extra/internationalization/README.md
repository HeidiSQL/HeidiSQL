# Transifex Client

## Installation

### Installing with a script (Linux/Mac)
You can install the latest Transifex CLI by executing:

```
curl -o- https://raw.githubusercontent.com/transifex/cli/master/install.sh | bash
```

Or you can isntall a specific version if you need by executing:

```
curl -o- https://raw.githubusercontent.com/transifex/cli/master/install.sh | bash -s -- yourVersion
```


This script will:
* Try to find the correct version for your system.
* Download & extract the CLI to the current folder.
* Check for a profile in one of `.profile, .bashrc, .bash_profile, .zshrc` and append `export PATH="<PWD result>:$PATH"`, so you can call 'tx' from any path.

**Note:** You need to restart your terminal for the `PATH` changes to be applied.

### Download from Github Releases (Linux/Mac/Windows)

Another way to install the Transifex CLI is to download
the latest version of the binary from GitHub
[here](https://github.com/transifex/cli/releases).

Choose the binary according to your system, download it and unzip it.
Copy the binary into the location you want and update the `PATH` variable
of your system if necessary.

The other way to install Transifex CLI in the system is to use the code.

Clone the [repository](https://github.com/transifex/cli) and go into the directory

```shell
cd /path/to/transifex/cli
```
### Building from source

The default way to build the binary is

  ```shell
  make build
  ```
This method requires to have golang in your system. It compiles Transifex CLI and
moves it into the `./bin/` directory of the repository.

If you don't have golang installed, but you have Docker enabled, you can use
the following command:

  ```shell
  make docker-build
  ```

This will build the binary and it will copy it at `./bin/` in the repository.

### Running from Docker (beta)

You can skip the installation and run the Transifex client from Docker if it is
available in your system. All you have to do is put this line:

```sh
alias tx='touch ~/.transifexrc; docker run --rm -i -t -v `pwd`:/app -v ~/.transifexrc:/.transifexrc -v /etc/ssl/certs/ca-certificates.crt:/etc/ssl/certs/ca-certificates.crt transifex/txcli --root-config /.transifexrc'
```

to your `~/.bashrc` / `~/.zshrc`. (The first time you use it you will have to
wait for a ~5MB download)


### Running from Github actions (beta)

You can invoke the CLI from within a Github workflow by using our Github
action. See the instructions [here](https://github.com/transifex/cli-action).

## Migrating from older versions of the client

The current version of the client maintains backwards compatibility for the `tx push`
and `tx pull` commands. So, if you have a CI setup that uses them, you should not have
to change anything. However, some things need to be different in the configuration files:

The section headers in `.tx/config` need to be different to also store the organization slug.
So after the migration `<project>.<resource>` should become `o:<org>:p:<proj>:r:<res>`.
In case something fails during this process, we will provide a message with the failed
migrated resource so that you can identify and change the section header manually.

You will be prompted for an API token in case you are using a username/password pair in
your `~/.transifexrc` file or if you are not using one.

If you are migrating an existing software project from an older version of the Transifex
client, you need to run:

```
tx migrate
```

This will take care of all the changes and create a back up file of the original config
in the same folder as `config_yyyymmddhhss.bak` before we start the migration process.

### Differences With the Previous Version

The two clients have some distinct differences when looking under the hood.
The new client is using Go instead of Python
  * for speed and
  * for the ability to produce binary files
  for multiple platforms.

Additionally, client is using APIv3 instead of APIv2 because
  * it is faster (calls occur asynchronously and you don't have to wait
    for parsing to finish) and
  * APIv2 is getting deprecated.

`Init`

The new client's init command creates the `.tx` folder in the current path,
and the config file with the following content which is required for the configuration:

```shell
[main]
host=https://app.transifex.com
```

In case there is already a `.tx/config` file in the current directory, the users
will get a prompt that informs them that, if they proceed, the contents of their
`.tx/config` file will be overridden. A `y/n` answer, is needed to proceed or abort.

`Add`

For the previous client, parts of functionality in `tx config` command adds resources
locally.

In the new client, this command is responsible to add a resource in the local config file.
Note that it needs all `organization`, `project` and `resource` slugs in order to build
the resource id for the APIv3.

It will create a new section in the `.tx/config` file for a resource like:

```ini
[o:org_slug:p:project_slug:r:resource_slug]
...
```

`Push`

The differences of the new client, are summarized here:

* resource IDs, can be accepted without the `-r` flag
* when neither `-s/-t` are set, `-s` is assumed
* `--all` flag creates new languages on Transifex if
  local files exist for them (on previous client this was the default behavior,
  now it needs the `--all` flag)
* without `--all` or `--languages`, the only languages that are considered are
  the intersection of local and remote languages


`Pull`

* resource IDs, can be accepted without the `-r` flag
* when neither `-s/-t` are set, `-t` is assumed
* without `--all` or `--languages`, the only languages that are considered are
  the intersection of local and remote languages
* `--json` download files (translations) as json files
* `--content_encoding/-e` The encoding of the file. This can be one of the following:
  * text (default)
  * base64

## Usage

### Initialising a Project

The first thing we need to do is run:

```
tx init
```

This will simply create an empty `.tx/config` file to mark the current folder
as a _Transifex project_. Your directory structure should now look like this:

```
- my_project/
  |
  + .tx/
  |  |
  |  + config
  |
  + locale/
    |
    + en.php
```
### Using Environment Variables
The available environment variables for the CLI:

* `TX_TOKEN`: The api token to use
* `TX_HOSTNAME`: The API hostname
* `TX_CACERT`: Path to CA certificate bundle file

You can either add these variables in your CI settings, your profile file or when executing the commands like:
`TX_TOKEN=myapitoken tx pull`

### Adding Resources to Configuration

We will add the php file as a source language file in our local configuration. The simplest way to do this is with `tx add` which will start an interactive session:

```
The Transifex Client syncs files between your local directory and Transifex.
The mapping configuration between the two is stored in a file called .tx/config
in your current directory. For more information, visit
https://developers.transifex.com/docs/cli#installation.

What is the path of the source file? locale/en.php

Next, we’ll need a path expression pointing to the location of the
translation files (whether they exist yet or not) associated with
the source file. You should include <lang> as a
wildcard for the language code.

What is your path extension? locale/<lang>.php

Use the arrow keys to navigate: ↓ ↑ → ←  and / toggles search
? Which organization will this resource be part of?:
  > Organization 1 (organization-1)
    Organization 2 (organization-2)
    Organization 3 (organization-3)
    Organization 4 (organization-4)
↓   Organization 5 (organization-5)


Use the arrow keys to navigate: ↓ ↑ → ←  and / toggles search
? Which project will this resource be part of?:
  > Project 1 (project-1)

Use the arrow keys to navigate: ↓ ↑ → ←  and / toggles search
? Which is the resource for this file?:
  > en.php (en_php)
    Create a new resource ()

SUCCESS  Your configuration has been saved in '.tx/config'
    You can now push and pull content with 'tx push' and 'tx pull'

```


Your `.tx/config` file should look like this:

```ini
[main]
host = https://app.transifex.com

[o:organization-1:p:project-1:r:en_php]
source_file = locale/en.php
file_filter = locale/<lang>.php
type = PHP
resource_name = Web Application
```

You can skip steps from the interactive session by adding flags to the `tx add`
command. In fact, you can skip the interactive session entirely if you provide
all the flags:

```
→ tx add \
    --file-filter=locale/<lang>.php \
    --type=PHP \
    --organization=organization-1 \
    --project=project-1 \
    --resource=en_php \
    --resource-name='Web Application' \
    locale/en.php
```

#### Adding resources in bulk

> With the old client I could add multiple resources at the same time with `tx
> config mapping-bulk`. What should I do now?

We decided not to implement this functionality in the new client because its
usage was complicated and it couldn't satisfy every user's need anyway. You can
add multiple resources with a relatively simple shell script. For example:

1. Add every subfolder of a `locale` folder as a resource:

   ```sh
   for FOLDER in $(ls locale); do
       # Exclusion list
       if echo "excluded_a excluded_b" | grep -w -q $FOLDER; then
           continue
       fi

       tx add \
           --organization org \
           --project proj \
           --resource $FOLDER \
           --file-filter "locale/$FOLDER/<lang>.po" \
           --type PO \
           "locale/$FOLDER/en.po"
   done
   ```

2. Add specific folders as resources:

   ```sh
   for FOLDER in $(echo path/to/folder_a path/to/folder_b path/to/folder_c); do

       # path/to/folder_a => path_to_folder_a
       RESOURCE_SLUG=$(echo $FOLDER | tr '/' '_')

       tx add \
           --organization org \
           --project proj \
           --resource $RESOURCE_SLUG \
           --file-filter "$FOLDER/<lang>.po" \
           --type PO \
           "$FOLDER/en.po"
   done
   ```

3. Turn every `.po` file into a configured resource:

   ```sh
   for FILEPATH in $(find . -name '*.po'); do

       # ./examples/locale/en.po => examples/locale/en.po
       FILEPATH=$(echo $FILEPATH | sed 's/^\.\///')

       # examples/locale/en.po => examples_locale
       RESOURCE_SLUG=$(echo $FILEPATH | sed 's/\/[^\/]*$//' | tr '/' '_')

       # examples/locale/en.po => examples/locale/<lang>.po
       FILE_FILTER=$(echo $FILEPATH | sed 's/[^\/]*$/<lang>.po/')

       tx add \
           --organization org \
           --project proj \
           --resource $RESOURCE_SLUG \
           --file-filter "$FILE_FILTER" \
           --type PO \
           $FILEPATH
   done
   ```

#### Adding remote resources in bulk

If you have content already setup in Transifex, you may want to setup local
resources in order to pull the language files on your system. In order to do
that, you can run the following command with Linux or Mac OS:

```sh
tx add remote \
    --file-filter 'translations/<project_slug>.<resource_slug>/<lang>.<ext>'
    https://app.transifex.com/myorganization/myproject/dashboard/
```

> The use of tx add remote appends the content in the .tx/config file and does not overwrite it. However, if the project and resource exist in the .tx/config file, then it will overwrite the previous information for the specific project & resource.

For Windows OS, please use double quotes instead of single quotes in the following example.

This will create entries in your configuration file for each resource in your
remote project. ie the configuration file may look like this:

```ini
[main]
host = https://app.transifex.com

[o:myorganization:p:myproject:r:resource1]
file_filter = translations/myproject.resource1/<lang>.po
source_file = translations/myproject.resource1/en.po
type = PO
minimum_perc = 0
resource_name = Resource 1

[o:myorganization:p:myproject:r:resource2]
file_filter = translations/myproject.resource2/<lang>.json
source_file = translations/myproject.resource2/en.json
type = KEYVALUEJSON
minimum_perc = 0
resource_name = Resource 2

[o:myorganization:p:myproject:r:resource3]
file_filter = translations/myproject.resource3/<lang>.html
source_file = translations/myproject.resource3/en.html
type = HTML
minimum_perc = 0
resource_name = Resource 3
```

The options for this command are:

- `--file-filter`: What to use as the file_filter and source_file options in
  the resulting configuration. This is a pattern that accepts the following
  parameters:

  - `<project_slug>`
  - `<resource_slug>` _(required)_
  - `<lang>` _(required)_ - can be used multiple times in the filter path
  - `<ext>`

  The default value for this option is
  `translations/<project_slug>.<resource_slug>/<lang>.<ext>` (the one we showed
  in our example)

- `--minimum-perc`: What to use as the minimum_perc option in the resulting configuration

- One or more project URLs.

After setting things up, you can pull the source files with `tx pull --source`.

### Pushing Files to Transifex

`tx push` is used to push language files (usually source language files) from
your machine to Transifex. You will most likely want to do that frequently
during the lifetime of you project when new source strings are introduced or
existing ones are changed. This will make the new strings available to
translators as soon as possible.

The simplest invocation of `tx push` is simply:

```sh
→ tx push
```

This will attempt to push the source file of all local resources that have been
configured with `tx add`.

**Limiting resources:**

You can limit the resources you want to push with:

```sh
→ tx push [RESOURCE_ID]...
```

A resource ID must refer to a resource that has already been configured with
`tx add` and has the form `<project>.<resource>`. So, if the URL of your
resource in Transifex is
`https://app.transifex.com/myorganization/myproject/myresource`, then the
resource ID will be `myproject.myresource`.

You can also use the `*` character to select multiple resources with the same
pattern. So, for instance, if you have the `abc.def` resource ID in your
configuration, you can select it with either `abc.*`, `*.def`, `ab*ef` or even
`a*.d*f`.

> Note: for backwards compatibility with previous versions of the client, you
> can also use the `-r/--resources` flag. You can also use both at the same
> time:
>
> ```sh
> → tx push p1.r1 p1.r2 -r p1.r3,p1.r4
> # Equivalent to
> → tx push p1.r1 p1.r2 p1.r3 p1.r4
> ```

`tx push` will create the resources on Transifex if they are missing.

**Language management:**

By default, the client will push the source file (the file that's being pointed
to by the `source_file` configuration option from `tx add`). If you use the
`-t/--translation` flag, `tx push` will push translation files. This may be
desirable if, for example, you previously pulled translation files with the
`--mode translator` option, translated using an offline translation tool and
now you want to push your work to Transifex, or if you are migrating from
another localization management service to Transifex. If you use both the `-t`
_and_ the `-s/--source` flags, then you will push both the source file and the
translation files.

When you use `-t`, the client will find all local files that match the
`file-filter` configuration option. The files that are found, and their language
codes constitute the _local_ languages. By default, the client will ask the
Transifex API for the languages that are supported by the project you are
pushing to (the _remote_ languages) and will only push languages that are both
_local_ and _remote_ (aka the **intersection** of _local_ and _remote_
languages).

You can use the `-l/--languages` flag to handpick which languages you want to
push. It only makes sense to include _local_ languages with the `-l` flag, ie
languages for which a file exists according to the `file-filter` configuration
option. The client will then push **only** the language files you have
specified. If you specify _local_ languages that are not yet supported by the
remote Transifex project, the client will attempt to add these languages to the
project first. Be careful of this since it may affect your pricing if you are a
paying customer.

```sh
→ tx push -t -l fr,de,pt_BR
```

The `-a/--all` flag will attempt to push **all** _local_ languages to the
remote Transifex project, adding them if necessary. Essentially, `-a` is
equivalent to using `-l` with all the _local_ language codes.

Transifex uses the _ISO/IEC 15897_ standard for language codes (for example
`en_US`). If you use a different format for the _local_ language codes, you can
define a mapping in your configuration file `.tx/config`. You
can specify these mappings for all configured resources by adding them to the
`[main]` section or you can specify mappings per resource. The "per-resource"
mappings take precendence. Configuring a language mapping looks like this:

```ini
# ...
[o:myorganization:p:myproject:r:myresource]
source-file = ...
# ...
lang_map = pt_PT: pt-pt, pt_BR: pt-br
```

This means that the _remote_ `pt_PT` language code maps to the _local_ `pt-pt`
language code and the _remote_ `pt_BR` language code maps to the _local_
`pt-br` language code.

The REMOTE_CODE is the language code supported by Transifex. And the LOCAL_CODE is your
language code.

The `-l` flag works with both _local_ and _remote_ language codes.

**Skipping pushing older files:**

The default behavior of the `tx push` command is to skip pushing a file when
the remote resource on Transifex has had a change more recently than when the
local file was last edited. To make sure that the local files are pushed even
if they are older than the remote resource, use the `-f/--force` flag.

You can use the `--use-git-timestamps` flag to compare against the last time
the local files were *committed* to the local git repository instead of the
last modification time in the filesystem. This can be useful in cases where you
have just cloned a repository or pulled a branch. In this case, the filesystem
modification time will reflect the time you pulled and not the time the file
was edited by an actual person. If you use the `--use-git-timestamps` flag and
no information about a local git repository can be found, then the client will
fall back to taking the filesystem timestamp into account.

**Other flags:**

- `--xliff`: Push xliff files instead of regular ones. The files must be
  located **in the same place** as indicated by the `file-filter` configuration
  option, but with the added `.xlf` suffix (`tx pull`ing with the `--xliff`
  option will put xliff files in the correct positions so you will probably not
  have to do this by hand)

- `--branch`: Using this flag, you can access copies of the regular remote
  resource that are tied to the provided branch. So if `tx push proj.res`
  pushes to the `https://app.transifex.com/org/proj/res` resource, then `tx
  push --branch foo proj.res` will push to the
  `https://app.transifex.com/org/proj/foo--res` resource. This way you can
  separate the localization effort across different branches. If you supply an
  empty string as the branch (`--branch ''`), then the client will attempt to
  figure out the currently active branch in the local git repository. For
  example:

  ```sh
  → git checkout -b new_feature
  → # Edit some source code files
  → # Extract source strings into language file
  → tx push --branch 'new_feature' myproject.myresource
  → # Or
  → tx push --branch '' myproject.myresource
  ```

  This way, the "regular"
  `https://app.transifex.com/myorganization/myproject/myresource` resource will
  not be affected by the changes you did to the source strings and the
  localization effort can be done in parallel on the
  `https://app.transifex.com/myorganization/myproject/new_feature--myresource`
  resource.

  > Note: Starting from version 1.5.0 resources created using the `--branch` flag,
  will have an enhanced functionality in transifex and will be able to automatically
  be merged into their bases. Resources created using the `--branch`  prior to this
  version, need to be pushed again in order for the new functionality to be available.

  ```sh
  → tx push --branch 'new_feature' --base '' myproject.myresource
  ```

- `--base`: Define the base branch when pushing a branch.

- `--skip`: Normally, if an upload fails, the client will abort. This may not
  be desirable if most uploads are expected to succeed. For example, the reason
  of the failed upload may be a syntax error in _one_ of the language files. If
  you set the `--skip` flag and an upload fails, then the client will simply
  print a warning and move on to the next language file.

- `--workers/-w` (default 5, max 30): The client will push files in parallel to improve
  speed. The `--workers` flag sets the number of concurrent uploads possible at
  any time.
- `--silent`: Reduce verbosity of the output.

- `--replace-edited-strings`: If present, source strings that have been edited
  (in the editor UI or via the API) will not be protected from this source
  file push and will instead be replaced. This can also be set on a
  per-resource level in the configuration file.

- `--keep-transations`: If present, translations of source strings with the
  same key whose content changes will not be discarded. This can also be set on
  a per-resource level in the configuration file.

### Pulling Files from Transifex

`tx pull` is used to pull language files (usually translation language files) from
Transifex to your machine. Most likely, you will do this regularly when you want to
incorporate newly available translations from Transifex into it.

The simplest invocation of `tx pull` is simply:

```shell
→ tx pull
```

This will attempt to pull the translation files of all local resources that have been
configured with `tx add`.

**Limiting resources:**

You can limit the resources you want to pull with:

```shell
→ tx pull [RESOURCE_ID]...
```

As stated in the `tx push` section, a resource ID must refer to a resource that has
already been configured with `tx add` and has the form `<project>.<resource>`.

You can also use the `*` character to select multiple resources with the same
pattern. So, for instance, if you have the `abc.def` resource ID in your
configuration, you can select it with either `abc.*`, `*.def`, `ab*ef` or even
`a*.d*f`.

> Note: for backwards compatibility with previous versions of the client, you
> can also use the `-r/--resources` flag. You can also use both at the same
> time:
>
> ```sh
> → tx pull p1.r1 p1.r2 -r p1.r3,p1.r4
> # Equivalent to
> → tx pull p1.r1 p1.r2 p1.r3 p1.r4
> ```

**Language management:**

By default, the client will pull the translation files of the existing files in the paths
that are defined in the `file_filter` configuration option from `tx add`.

For instance, if the directory structure looks like this:

```
- my_project/
  |
  + .tx/
  |  |
  |  + config
  |
  + locale/
    |
    + en.php
```

and the `.tx/config` contains:

```ini
source_file = locale/en.php
file_filter = locale/<lang>.php
```

If you use the `-s/--source` flag, `tx pull` will pull the source file that is
pointed from the `source_file` option of the config file.

If you use both the `-t/--translation` _and_ the `-s/--source` flags,
then you will pull both the source file, and the translation files.

Then the client will try to search for any existing language file located
at the `locale/<lang>` path (where `<lang>` is the language code) and will
try to update it, for example `locale/el.php`, `locale/fr.php`, etc.

In case that there aren't any translation files, like in the structure above,
then you must either use the `-l/--language` or the `-a/--all` flag.

Use the `-l/--languages` flag to handpick which languages you want to
pull. It only makes sense to include _remote_ languages with the `-l` flag, ie
languages for which a file does not exist according to the `file_filter`
configuration option. The client will then pull **only** the language
files you have specified:

```shell
tx pull -l el,fr,nl
```

> Note:
> The languages that are defined with the -l/--language flag
> should belong to the project for the client to download them.

The `-a/--all` flag will attempt to pull **all** languages from the
remote Transifex project. Essentially, `-a` is equivalent to using
`-l` with all the project language codes.

As stated before, Transifex uses the _ISO/IEC 15897_ standard for
language codes. If you use a different format for the _local_ language
codes, you can  define a mapping in your configuration file `.tx/config`.
You can specify these mappings for all configured resources by adding them
to the `[main]` section or you can specify mappings per resource.
The "per-resource" mappings take precendence. Configuring a language mapping
looks like this:

```ini
# ...
[o:myorganization:p:myproject:r:myresource]
source-file = ...
# ...
lang_map = pt_PT: pt-pt, pt_BR: pt-br
```

This means that the _remote_ `pt_PT` language code maps to the _local_ `pt-pt`
language code and the _remote_ `pt_BR` language code maps to the _local_
`pt-br` language code.

The `-l` flag works with _remote_ language codes.

**Skipping pulling older files:**

The default behavior of the `tx pull` command is to skip pulling a file when
a local file on a machine has had a change more recently than when the
remote resource was last edited. To make sure that the remote resources
are pulled even if they are older than the local files, use the `-f/--force` flag.

You can use the `--use-git-timestamps` flag to compare against the last time
the local files were *committed* to the local git repository instead of the
last modification time in the filesystem. This can be useful in cases where you
have just cloned a repository or pulled a branch. In this case, the filesystem
modification time will reflect the time you pulled and not the time the file
was edited by an actual person. If you use the `--use-git-timestamps` flag and
no information about a local git repository can be found, then the client will
default to taking the filesystem timestamp into account.

**Other flags:**

- `--xliff`: Pull xliff files instead of regular ones. The files will be
  placed **in the same place** as indicated by the `source-file` and
  `file-filter` configuration options, but with the added `.xlf` suffix.

- `--json`: Pull translation files as json instead of regular ones. As above,
  the files will be placed **in the same place** as indicated by the `file-filter`
  configuration options, but with the added `.json` suffix. Currently, source
  files are not supporting json format.

- `--disable-overwrite`: If a file exists do not update it. This is useful
  when using `-a/--all` flag and you don't want to change the existing files
  but only download other language files.

- `--keep-new-files`: Used with `--disable-ovewrite` to create new files
	if file already exists locally with a '.new' extension.

- `--mode/-m`: The translation mode of the downloaded file. This can be one of the
following: `'default', 'reviewed'`, `'proofread'`, `'translator'`, `'untranslated'`,
 `'onlytranslated'`, `'onlyreviewed'`, `'onlyproofread'`, `'sourceastranslation'` **(default
 mode is: **`'default'`). Use like` 'tx pull -m proofread'` to download only proofread
 translations.

- `--branch`: Using this flag, you can access copies of the regular remote
  resource that are tied to the provided branch. So if `tx pull proj.res`
  pulls from the `https://app.transifex.com/org/proj/res` resource, then `tx
  pull --branch foo proj.res` will pull from the
  `https://app.transifex.com/org/proj/foo--res` resource. This way you can
  separate the localization effort across different branches. If you supply an
  empty string as the branch (`--branch ''`), then the client will attempt to
  figure out the currently active branch in the local git repository. For
  example:

  ```sh
  → git checkout new_feature
  → # Get updated files for this branch
  → tx pull --branch 'new_feature' myproject.myresource
  → # Or
  → tx pull --branch '' myproject.myresource
  ```

  This way, the "regular"
    `https://app.transifex.com/myorganization/myproject/myresource` resource will
  not be affected by the changes one did, and the localization effort can be done
  in parallel on the
  `https://app.transifex.com/myorganization/myproject/new_feature--myresource`
  resource.

- `--skip`: Normally, if a download fails, the client will abort. This may not
  be desirable if most downloads are expected to succeed. For example, the reason
  of the failed download may be a syntax error in _one_ of the language files. If
  you set the `--skip` flag and an upload fails, then the client will simply
  print a warning and move on to the next language file.

- `--minimum-perc=MINIMUM_PERC` Specify the minimum translation completion
  threshold required in order for a file to be downloaded.

- `--workers/-w` (default 5, max 30): The client will pull files in parallel to improve
  speed. The `--workers` flag sets the number of concurrent downloads possible at
  any time.

- `--pseudo`: Generate mock string translations with a ~20% default length increase in characters.

- `--silent`: Reduce verbosity of the output.

### Removing resources from Transifex
The tx delete command lets you delete a resource that's in your `config` file and on Transifex.

To delete a resource, use the following command:
```
tx delete <project_slug>.<resource_slug>
```

To delete all resources in a specific project at once, instead of referring to a specific resource_slug, you can use the asterisk character as follows:
```
tx delete project_slug.*
or
tx delete project_slug.\*
```

> Note: for backwards compatibility with previous versions of the client, you
> can also use the `-r/--resources` flag. You can also use both at the same
> time:
>
> ```sh
> tx delete -r <project_slug>.<resource_slug> ....
> ```

**Other flags:**
- `--skip`: Normally, if a delete fails, the client will abort. This may not
  be desirable if most deletes are expected to succeed. For example, the reason
  of the failed delete may be a a resource that has translated content. If
  you set the `-s/--skip` flag and an delete fails, then the client will simply
  print a warning and move on to the next resource.
- `--force`: In case you want to proceed to a deletion even if resources have
  translations use the `-f/--force` flag.
- `--branch`: In case you want to delete a resource's branch that is on Transifex.
  If you supply an empty string as the branch (`--branch ''`), then the client
  will attempt to figure out the currently active branch in the local git repository.

### Merging Resource
The tx merge command lets you merge a branch resource with its base resource (applies only to resources created with the `--branch` flag)

To merge a resource to its base resource, use the following command:
```
tx merge --branch branch_name project_slug.resource_slug
```
**Other flags:**
- `--conflict-resolution`: Set the conflict resolution strategy. Acceptable options are `USE_HEAD` (changes in the HEAD resource will be used) and `USE_BASE` (changes in the BASE resource will be used)
- `--force`: In case you want to proceed with the merge even if the source strings are diverged, use the `-f/--force` flag.

### Getting the local status of the project
The status command displays the existing configuration in a human readable format. It lists all resources that have been initialized under the local repo/directory and all their associated translation files:

```
tx status
myproject -> default (1 of 1)
Translation Files:
 - en: po/smolt.pot (source)
 - ar: po/ar.po
 - as: po/as.po
 - bg: po/bg.po
 - bn_IN: po/bn_IN.p
 ...
 ```

 To get the status of specific resources just add the resources you want in your command:

 ```
 tx status <project_slug>.<resource_slug> ....
 ```

> Note: for backwards compatibility with previous versions of the client, you
> can also use the `-r/--resources` flag. You can also use both at the same
> time:
>
> ```sh
> tx status -r <project_slug>.<resource_slug> ....
> ```

### Updating the CLI app
The `tx update` command provides a way to self update the application without going to Github releases page.

 ```
 tx update
 ```

 **Flags:**
- `--check`: Check if there is a new release. Nothing gets updated.
- `--no-interactive`: Proceed to update if there is a newer version without seeing the confirmation prompt.
- `--debug`: Enable logging for the binary update process.
# License

Licensed under Apache License 2.0, see [LICENSE](LICENSE) file.
