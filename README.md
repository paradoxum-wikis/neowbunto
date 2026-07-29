# Neowbunto

This repository contains the Neowtext engine ran on Scribunto, while the one that runs natively on JavaScript is integrated tightly inside the [Statistics Editor](https://github.com/paradoxum-wikis/Statistics-Editor).

The final iteration of the old Neowbunto that was written entirely in Lua can be found [here](https://tds.fandom.com/wiki/Module:Neowbunto?oldid=572606).

## Build

Make sure you have Lua (any version above 5.1 is fine).

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
