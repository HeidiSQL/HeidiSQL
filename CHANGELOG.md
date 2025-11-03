# Change Log

Notable changes up to the last release.

## [12.13.1.1] - 2025-11-03

### ⛰️  Features

- Sort session folders at top per setting - ([93dc936](https://github.com/HeidiSQL/HeidiSQL/commit/93dc93680327beb4e24422e7cae9c97b71a9ba24))
- Filter box in user manager - ([b57b764](https://github.com/HeidiSQL/HeidiSQL/commit/b57b76443bfc6c31dd81ea50d7c0238278f78301))
- Add checkbox "Open file after creation" to grid export dialog - ([613f4d8](https://github.com/HeidiSQL/HeidiSQL/commit/613f4d8a23f80fe9542b90724a1cab5e6e9f96b6))
- Allow sorting columns in table designer - ([8541eb4](https://github.com/HeidiSQL/HeidiSQL/commit/8541eb404de942a0b3e976201430cbc65964887a))

### 🚀  Enhancements

- Make file exit action OS friendly - ([1f19d00](https://github.com/HeidiSQL/HeidiSQL/commit/1f19d004905a39a6a1d49df698bd4194735aa29c))

### 🐛 Bug Fixes

- List procedures in PostgreSQL - ([db902f7](https://github.com/HeidiSQL/HeidiSQL/commit/db902f7da1133c05bcaf67ca92e273582328fdf3))
- Missing DELETE HISTORY privilege on MariaDB - ([f7b80db](https://github.com/HeidiSQL/HeidiSQL/commit/f7b80dbb51f802e166528308f35106ada0ae0883))
- Prevent crash in auto-refresh action - ([2c25e04](https://github.com/HeidiSQL/HeidiSQL/commit/2c25e04baf90d3912749421fadac798b7d1db786))
- Bad message with no library selected - ([f9a486b](https://github.com/HeidiSQL/HeidiSQL/commit/f9a486b40352da9426f2575579271d25ffb16ec5))
- Generate missing values for geometry columns - ([eb1b5ea](https://github.com/HeidiSQL/HeidiSQL/commit/eb1b5eac59100605b0a5b749037a6297ff9d6fb7))
- Allow modify length of index with binary column - ([173efeb](https://github.com/HeidiSQL/HeidiSQL/commit/173efeb6aa4f920c14c961aa3458cfe2fbddf2fd))
- End global "edit function" mode for grid editing early - ([5cac850](https://github.com/HeidiSQL/HeidiSQL/commit/5cac85089a58dcc8e11c540e1499e001df76d69a))
- Take care for escaped ENUM definitions - ([328fb72](https://github.com/HeidiSQL/HeidiSQL/commit/328fb7287b6597a663c465bcece8c05218753f13))
- Support return data type of stored function containing white spaces - ([d7b1faa](https://github.com/HeidiSQL/HeidiSQL/commit/d7b1faa637869472ffbdabd28d5885caa3a33dc9))
- Safety replacement for folder separator when renaming a session or folder - ([18a21ef](https://github.com/HeidiSQL/HeidiSQL/commit/18a21ef9e4f1f450391b40a1cb8ee3aef036fa36))
- Leave away schema when double-click table for inserting into query editor - ([b72f259](https://github.com/HeidiSQL/HeidiSQL/commit/b72f2595c55644daf55b76086ab38f99fa3d93ce))
- Sticky empty-password warning after setting a non-empty one - ([f2028e1](https://github.com/HeidiSQL/HeidiSQL/commit/f2028e135e4e3ded46612d30b4687db413c2bb0f))
- Support double dollar quotes on PostgreSQL - ([caeae88](https://github.com/HeidiSQL/HeidiSQL/commit/caeae88f25d7b7118836f22c7223a24523a0ba2d))

### 📚 Documentation

- *(CHANGELOG)* Proper changelog - ([79bb279](https://github.com/HeidiSQL/HeidiSQL/commit/79bb279a15998756d040ad3830b7e7bb5f27f8cc))

### ⚙️ Miscellaneous Tasks

- Bumping version to 12.13.1.1 - ([bf23332](https://github.com/HeidiSQL/HeidiSQL/commit/bf23332cc1731f521330a61e7fb5cebff87687c3))
- Using correct target of `build-*` - ([16a54a5](https://github.com/HeidiSQL/HeidiSQL/commit/16a54a57aa6974c819801c3e304367fab7ec07af))
- Fixing some blantant mistakes - ([76dfa98](https://github.com/HeidiSQL/HeidiSQL/commit/76dfa98217d4eabaf30e96a938fb982fdabb221d))
- New category: Enhancements - ([dd1ad34](https://github.com/HeidiSQL/HeidiSQL/commit/dd1ad34317215a16178fc6bf5de04146358e29aa))
- Removing unnecessary `tx-push` recipe - ([d293d64](https://github.com/HeidiSQL/HeidiSQL/commit/d293d648d8cc933d01e269619cc041f2833b6a3f))
- Forgot to copy `.ini` files on `run-*` - ([c513e53](https://github.com/HeidiSQL/HeidiSQL/commit/c513e535d916d414312481828bdc5a241047a7db))
- Completing `run-gtk2`, `run-qt5` - ([04650d9](https://github.com/HeidiSQL/HeidiSQL/commit/04650d93a7c0a24e8c53b800791b9511d6a1bb30))
- Adding empty `tx-push` to `Makefile` - ([3ceed39](https://github.com/HeidiSQL/HeidiSQL/commit/3ceed3936c24f3b9034b558a16c490f826c90b34))
- Adjusting workflow to skip secrets detection - ([14dd9ce](https://github.com/HeidiSQL/HeidiSQL/commit/14dd9ce9a590e9e1a768f32db1dcb6412c857eab))
- Adding usage of `secrets.mk` - ([123a2e7](https://github.com/HeidiSQL/HeidiSQL/commit/123a2e7d36f0478cd8e9ce24b6c25ed85386517b))
- Forgot to change the output folder - ([78f1415](https://github.com/HeidiSQL/HeidiSQL/commit/78f1415c210c9bea2fdb0edf27cfa1dd89909f15))
- First batch of changes - ([2bee126](https://github.com/HeidiSQL/HeidiSQL/commit/2bee126353676a3387cfd10ae4f348a2488e5938))


