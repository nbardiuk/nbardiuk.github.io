---
title: "Vim as HTTP client"
date:  2020-10-05 00:00:00
---

Let me share a quick tip on how to use Vim with curl as a simple HTTP client.

<video width="95%" autoplay loop>
  <source src="/videos/curl.webm" type="video/webm"/>
</video>

In the core of this tip is a filter through curl.

```vim
:!curl --config -
```

Let us walk through this.

## Vim filter


[`:help filter`](https://vimhelp.org/change.txt.html#filter)

> A filter is a program that accepts text at standard input, changes it in some way, and sends it to standard output.

We can send some buffer lines to an external command, and Vim will replace them with the command output.

Given a buffer

```
3
2
4
1
```

After filtering lines through the sort command

```vim
:%!sort
```

The lines are in order.

```
1
2
3
4
```

## curl configuration file

Curl allows passing all command-line flags using a configuration file.

[`--config`](https://curl.haxx.se/docs/manpage.html#-K) 

> Specify a text file to read curl arguments from. The command line arguments found in the text file will be used as if they were provided on the command line.

Like many command-line apps, curl uses a hyphen (`-`) to make the app read file from standard input.

```sh
$ echo '--url http://ifconfig.me --silent' | curl --config -
1.2.3.4
```

Let's use this in Vim buffer.

```
--url http://ifconfig.me
--silent
--show-error
```

run the command

```vim
:%!curl --config -
```

The buffer now contains a curl response.

```
1.2.3.4
```

## Interactive approach

It's hard to iterate over request arguments because the curl command replaces them with the response.

To make it more interactive, we can duplicate the configuration lines before running the curl.

Below is a sequence of commands that can help with this

* `vip` select all configuration lines using paragraph text object
* `y` copy selected lines
* `P` paste before the cursor
* `gv` reselect the lines
* `:!curl --config -` run curl on the selection

Record this into a macro or create a command, and you have an interactive environment.

```vimrc
:map <leader>cc vipyPgvO<Esc>O<Esc>gv:!curl --config -<CR>
```

The extra bit `gvO<Esc>O<Esc>` inserts an empty line between request and response. It helps to make them different paragraphs, distinct text objects.

## Use a plugin

This approach is excellent for a quick hack or an example of using the Vim filter command.

I don't use it every day. Instead, I prefer to use a plugin like [Vim REST Console](https://github.com/diepm/vim-rest-console).
Compared to the simple curl command, it has several advantages:

* has simple HTTP syntax
* supports variables
* does not require escaping POST payload
* formats response payload
