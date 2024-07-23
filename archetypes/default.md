+++
title = '{{ replace .File.ContentBaseName "-" " " | title }}'
date = {{ .Date }}
draft = true
mathjax = true
categories = ['{{ replace .File.Dir "content/posts/" "" }}']
+++
