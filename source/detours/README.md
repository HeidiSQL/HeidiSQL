![Version](https://img.shields.io/badge/version-v2.2-yellow.svg)
![License](https://img.shields.io/github/license/MahdiSafsafi/DDetours)
![Lang](https://img.shields.io/github/languages/top/MahdiSafsafi/DDetours.svg)

The **DDetours** is a library allowing you to hook Delphi and Windows API functions. It provides an easy way to insert and remove hook.

## What's new in Version 2.2 ? ##
* Support for FPC and older Delphi version notably D7.
* Support for recursive section.
* Support for custom parameter/tag for each trampoline function.
* See CHANGELOG for complete changes.

## Features : ##
* Supports **x86** and **x64** architecture.
* Supports <u><b>multiple hook</b></u> for a single function.
* Supports Delphi 7/2005-2010/XE-Rio(Delphi 10.3).
* Supports Lazarus/FPC.
* Supports recursive function inside the hook function.
* Supports hooking interfaces methods by **MethodName** or **MethodIndex**.
* Supports COM **vtable** patching.
* Supports hooking object methods.
* Allows calling the original function via <u><b>Trampoline/NextHook</b></u> function.
* **COM**/**Interfaces**/**win32api** support.
* Thread-safe for hooking and unhooking.
* 64 bit address is supported.
* The library does not use any external library.
* The library can insert and remove the hook at any time.
* The library contains InstDecode library, that allows you to decode CPU instructions (x86/x64).

This project contains two sub project : **DDetours** and **InstDecode*** library.

The InstDecode Library is a library that can decode both (x86/x64) instructions. You can consider it as a small disassembler routine.
It can decode instruction and getting information about the instruction (size of instruction, displacement, immediate data, jump address,..) without displaying mnemonics making it very faster and very small in size.

These two libraries were coded in pure Pascal language with Delphi XE7.

See the [Wiki](https://github.com/MahdiSafsafi/DDetours/wiki) page for more information about how to use the library.

Please, if you find any bug, feel free to report it.
