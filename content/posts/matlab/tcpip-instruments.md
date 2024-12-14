+++
title = 'Tcpip Instruments in Matlab中连接和调试采用TCP/IP协议的仪器'
date = 2024-12-12T23:38:52+08:00
draft = true
mathjax = false
categories = ['matlab', 'instrument']
tags = ['matlab', 'methods', 'properties', 'tcpipclient', 'echotcpip', 'writeline', 'readline', 'configureTerminator', 'configureCallback']
toc = true
tocBorder = true
+++



    A:师兄，Matlab可以做什么？
    B:除了生孩子，什么都会。（这下你懂师兄的意思了吧）
    A:【星星眼】哦，那我还是可以帮兰陵王生孩子！
    B:【倒】

先来一点跟Matlab无关的内容，现在很多仪器做得非常先进，只需要两根线，一根电源线，一根网线（RJ45）。连接到交换机，就能通过TCP/IP或者UDP协议控制仪器、采集数据。这样的仪器通常内置了IP地址和物理地址，那么首先就有一个问题。当不同的预设IP的仪器要一起使用（在同一台电脑上进行采集），会发生需要跟不同网段的仪器连接的问题。这个时候就需要在电脑上设置路由。下面就是如何在Windows系统上设置路由的方法。

## 多个不同网段的仪器的连接

我们假设有两台仪器：

1. 191.30.88.40
2. 192.168.0.102

这两个仪器网段不同，就是说要求我们的电脑设置ip地址在两个网段上。这个时候就需要设置多路路由。

首先，打开网络的属性。具体在设置-网络和Internet-网络和共享中心。在这里点击，活动链接（图中蓝色的链接）。

![](/matlab-img/setup-routes/step1.png)

这样就打开了网络连接的状态窗口，在这里点击属性。

![](/matlab-img/setup-routes/step2.png)

打开属性窗口，选择Internet协议版本4（TCP/IPv4），双击，或者点击下方圈出的属性。

![](/matlab-img/setup-routes/step3.png)

在这个属性页面，我们设置一个仪器要求的网段。首先勾选使用下面的IP地址，填写：

- IP地址：191.30.88.1
- 子网掩码：255.255.255.0
- 默认网关：空着也没问题

然后点击高级。

![](/matlab-img/setup-routes/step4.png)

在高级设置页面，这里的已启用DHCP处就应该显示我们刚才设置的IP地址和子网掩码。在这里，增加另外一台仪器的IP地址和子网掩码。点击添加，填写：

- IP地址：192.168.0.1
- 子网掩码：255.255.255.0

![](/matlab-img/setup-routes/step5.png)

一路确定，就可以同时访问两个仪器。在cmd/powershell窗口测试：

```shell
ping 191.30.88.40
ping 192.168.0.102
```

## Matlab中的Tcpip对象

Matlab中的Tcpclient对象是用来连接和调试采用TCP/IP协议的仪器的。这里的理解就是，仪器相当于是一个服务器，我们的电脑相当于是一个客户端。我们通过Tcpclient对象来连接仪器，发送命令，接收数据。

### 创建Tcpipclient对象

```matlab
t = tcpclient('xx.xx.xx.xx', 5025);
```

这里最主要的就是IP地址和端口号，这两个信息请查看说明书或者给供货商致电询问。

有了这个对象，我们就能发送命令、接收数据。

我们可以执行`properties(t)`来查看这个对象的属性，执行`methods(t)`来查看这个对象的方法。这个仅仅作为练习。

记得这里，客户端对象的属性可以在创建的时候设置，也可以在创建之后设置。

- `t.Address`：IP地址，只能在创建的时候设置。
- `t.Port`：端口号，只能在创建的时候设置。
- `t.Timeout`：设置超时时间，单位是秒。可以在创建的时候设置，也可以在创建之后设置。
- `t.ConnectTimeout`：设置连接超时时间，单位是秒。可以在创建的时候设置，也可以在创建之后设置。

客户端对象还有些跟数据发送和读取有关系的属性，可以参考文档。

还有一对函数也是Matlab中很常用的，`set/get`。

```matlab
set(t, 'Timeout', 10);
get(t, 'Timeout');
```

还可以只带一个参数，`get`列出所有的属性和它们的值，`set`列出所有能够用`set`设置的属性。

```matlab
get(t)
                   Address: 'localhost'
                      Port: 8808
         NumBytesAvailable: 0
           NumBytesWritten: 0
            ConnectTimeout: Inf
       EnableTransferDelay: 1
                   Timeout: 10
                  UserData: []
                 ByteOrder: "little-endian"
                Terminator: "LF"
         BytesAvailableFcn: []
    BytesAvailableFcnCount: 64
     BytesAvailableFcnMode: "off"
          ErrorOccurredFcn: []
```

```matlab
set(t)
                   Timeout: {}
                  UserData: {}
                 ByteOrder: {}
                Terminator: {}
         BytesAvailableFcn: {}
    BytesAvailableFcnCount: {}
     BytesAvailableFcnMode: {}
          ErrorOccurredFcn: {}
```

下面就简单介绍一下Tcpclient对象的用法。

## `Tcpclient`对象的用法

### 测试服务器

当然，我们对仪器进行测试还是挺麻烦的，Matlab提供了一个回声服务器可以用于测试我们实现的客户端功能。

```matlab
{{% codesnap "/static/matlab-code/+tcpipInstr/startEchoServer.m" %}}
```

这个端口，开在了`localhost`的`8808`端口上。当我们不需要时，可以再次调用这个函数关闭服务器。


```matlab
{{% codesnap "/static/matlab-code/+tcpipInstr/stopEchoServer.m" %}}
```

### 模式一：字符串模式

对于字符串模式，我们可以使用`writeline`和`readline`方法。这里的读取和发送都涉及到一个终止符，这个终止符可以通过`configureTerminator`方法设置。

```matlab
t = tcpclient('localhost', 8808);
configureTerminator(t, "CR/LF");
writeline(t, "Hello, world!");
data = readline(t);
disp(data);
% Hello, world!
clear t;
```
这里`writeline`会自动添加终止符，`readline`会读取到终止符为止的数据并返回不包含终止符的数据。


### 模式二：二进制模式

对于二进制模式，我们可以使用`write`和`read`方法。这里的读取和发送都是字节流，没有终止符。

```matlab
t = tcpclient('localhost', 8808);
write(t, uint8([1, 2, 3, 4, 5]));
data = read(t, 5);
disp(data);

write(t, [4, 3, 2, 1 , 0], "double")
data = read(t, 5, "double");
disp(data);
% 4 3 2 1 0

clear t;
```

### 回调函数

我们可以通过`configureCallback`方法设置回调函数，当有数据到达时，会调用这个回调函数。此外，回调函数有3种类型：

- `byte`：当有字节到达时调用。
- `terminator`：当有终止符到达时调用。
- `off`：停止。


```matlab
t = tcpclient('localhost', 8808);

myCallback = @(src, ~)disp(readline(src));

configureCallback(t, "terminator", myCallback);

pause(1);
writeline(t, "Hello, world!");

clear t;
```


## 总结

1. 设置多路路由，可以同时访问不同网段的仪器。
2. Tcpclient对象是用来连接和调试采用TCP/IP协议的仪器的。
3. 回声服务器可以用于测试我们实现的客户端功能。
4. 我们也可以构造`tcpserver`对象，用于接收客户端的请求，留到下回。