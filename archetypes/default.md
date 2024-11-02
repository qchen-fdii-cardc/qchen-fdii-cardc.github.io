+++
title = '{{ replace .File.ContentBaseName "-" " " | title }}'
date = {{ .Date }}
draft = true
mathjax = false
categories = ['{{ path.Base .File.Dir }}']
tags = ['{{ path.Base .File.Dir }}']
toc = true
tocBorder = true
+++
