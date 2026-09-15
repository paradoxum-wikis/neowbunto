# Neowbunto

This repository contains the [Neowtext](https://tds.wiki/w/Help:Neowtext) engine ran on Scribunto, while the one that runs natively on JavaScript is integrated tightly inside the [Statistics Editor](https://github.com/paradoxum-wikis/Statistics-Editor).

The final iteration of the old Neowbunto that was written entirely in Lua can be found [here](https://tds.wiki/w/Module:Neowbunto?oldid=604652).

## Build

Make sure you have [Lua](https://www.lua.org/ftp) installed (any version from or above 5.1 is fine).

Bash:

```bash
git clone https://github.com/paradoxum-wikis/neowbunto.git
cd neowbunto
./fnl.sh setup
./fnl.sh build
```

PowerShell:

```powershell
git clone https://github.com/paradoxum-wikis/neowbunto.git
cd neowbunto
./fnl setup
./fnl build
```

You can then find the built neowbunto in the `dist` directory.

## License

[MIT](./LICENSE)
