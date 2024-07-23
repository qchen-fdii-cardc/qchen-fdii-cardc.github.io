+++
title = '{{ replace .File.ContentBaseName "-" " " | title }}'
date = {{ .Date }}
draft = true
mathjax = false
categories = ['{{ replace .File.Dir "content/posts/" "" }}']
+++
