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

Matlab中的Tcpipclient对象是用来连接和调试采用TCP/IP协议的仪器的。

### 创建Tcpipclient对象

```matlab
t = tcpipclient('xx.xx.xx.xx', 5025);
```

这里最主要的就是IP地址和端口号，这两个信息请查看说明书或者给供货商致电询问。

有了这个对象，我们就能发送命令、接收数据。