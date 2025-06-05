+++
title = 'OOP in Rust中的面向对象程序设计乱评'
date = 2025-06-05T17:27:47+08:00
draft = false
mathkatex = true
categories = ['rust', 'oop']
tags = ['rust', 'oop', 'trait', 'impl', 'adt', 'enum', 'struct']
toc = true
tocBorder = true
+++


## OOP想达到什么目的？

1967年诞生的面向对象程序设计（基于对象程序设计）、面向对象系统分析，在诞生后若干年都是非常热门和前卫的话题。虽然现在实际上也是很重要的议题和工具，但是对函数式、trait等其他的种种奇怪（并不是）的做法似乎可能貌似要把OOP挤到舞台的边角上。

虽然Rust、Kotlin、Go、Swift等一些编程语言变着花样擦边OOP，各种号称放弃OOP，OOP依然是程序设计和软件构建中的核心隐喻（Metaphor）。OOP现在看起来不热门，我感觉只是OOP浸入软件开发更加基础的思维底层。

就比如说这个`obj.func()`调用方式，曾经被当做OOP的典型和最为重要的标志。与之对应的是基于过程的语言中，`func(&obj)`这样的调用方式。

Java和C++两大OOP巨头，虽然每个人都讨厌什么GoF那一套子设计模式（我至今没有学完……），依然牢牢占据了市场。

因为构建一个软件，实在是一个很复合而且复杂的过程，既包含了认知、又包含了构造，严重地挑战着人类继承自洞穴人时代大脑、智能结构。人类，擅长析构胜于构造。以为析构的活动比创造简单和直观得多，把一头野兽肢解，得到骨头、血液、脂肪、肌肉，那些可以用来干什么，很自然。但是创造一个新式样的房屋，就比较难。

更别说，软件构建还需要考虑管理开发目标、开发需求变更，这变本加厉地增加直立猿点痛苦，使其耗能巨大、启动困难的有限大脑倍感深陷泥潭，让事情变得雪上加霜。

所以，建筑房屋、构造软件，很多时候除了工程还冠以艺术的名头。这也算是给干这个是情的人一点点特别的褒奖，毕竟跟工程比起来，艺术这样的混沌更适合我们人类，直觉人人都爱，拍脑袋人人都会。

那么，软件构造到底能不能足够工程？这就是OOA/OOP的目标，使得软件构造更加工程话。提供一种语法和语义，提供一系列词汇，抽象一些原则，确立一个过程（哪怕包含了迭代），让软件构造变得可控。

实际上，在计算时代的早期，程序员基于语句思考编程问题，到了1970$\sim$1980年代，开始了有子程序思考编程的方式，一直到21世纪，以类为基础思考编程问题才成为主流。

- 语句
- 子程序
- 类

这三个思维工具，映射在程序开发语言的发展过程中；也映射到软件系统的能力上。现在的软件系统已经能够解决超级复杂的问题。现在的高级软件能够很容易地写出以前最高级程序员才能编写的程序，例如LLVM那一套东西，比子程序时代的最高端程序员还是厉害多了……现在Python入门的初学者，掌握着语句编程时代大师才能挥舞的史诗专武……

## OOP难在哪里？

其实，基于对象编程非常的愉快；状态的变化封装在类和对象的内部，始终能够保持对象的一致性；对象自己完美处理自己的创建和销毁，管理自己的资源。当然，这都是理想的情况……设计地凑凑活活的类和对象们，通常会泄露自己的内存、失去自己的一致性、破坏自己管理的资源的生命周期……

### 设计的难点

设计良好的类系统是一个很难的事情。首先，软件的设计就是非常恶心的非常难的事情。

软件设计的问题是一个很险恶的问题，必须实现或者部分实现一个软件才能够明确地定义软件，也就是说只有实现一个软件才能完成一个软件的设计。只有完成一个类的设计才能完整地定义一个类。这就使得类系统的设计过程是一个没有章法的过程、必须自行确定取舍、调整顺序，还必须收到诸多的限制，设计的过程和结构都是不确定的，还必须一边实现、一边调整设计、随时根据实现的结果来调整设计……

大佬们用了一堆比喻来描述这个过程：

- 软件编制、编程就是写作：style、风格才是唯一个能够讨论的，可读性是一个很重要的议题；
- 软件编制、编程就是耕作和培育：实际上没有任何直接可靠的手段来控制软件实现的过程和结果……
- 软件编制、编程是一个养殖和生长的过程：一点点的增量式开发才是唯一可能的，这个思路为演进式开发和敏捷编程提供了思想基础；
- 房屋的建造是大部分时候用得更多的类比：机构、测试、构建、基础、分离这些属于都是从建筑行业带来的，最终被用于软件构造。

最终就是，软件框架就不是初级程序员容易理解的东西，但也是初级程序员或者OOP入门是必须面对的让人绝望的悬崖……

### 实现的难点

前面说过，必须部分或者全部设计才能明晰地定义设计。所以，实现也很是考验一个试图入门OOP的程序员。

这里有语言的问题，也有非语言的问题。决策和风格，同样在是现阶段让人头发难保。

C++和Java在这个部分都非常优秀。但是两者的优秀是完全不同的。Java的优秀是简单的胜利；C++的优秀是全能的胜利。一部分人对Java不满，这也不行，那也不行，憋憋屈屈；一部分人对C++不满，这也行，那也行，到底选哪个?

所以到这里，Rust（以及一部分后CPP/Java时代的语言）选择了另外一个OOP的实现思路。

## Rust中的OOP

我们前面强调了一点，要完整的定义OOP设计结果，通常不得不部分或者全部的实现OOP设计。

### OOP的调用方式

在Rust中，与C++/Java类似，对象的使用和调用是一样的。创建一个对象，这个对象包含了一些数据（有公开的数据、也有不公开的内部数据）和方法（公开的方法、也有不公开的内部方法）。当然，调用OOP的全部意义，就是可以保持内部数据的一致性，通过公开数据和公开方法来完成功能。

```rust
{{% codeseg "static/rust/oop-in-rust/src/main.rs" 35 37 %}}
```

这个调用方式，在Rust中，与C++/Java类似。这个字符串，很好地封装了其内存，看起来简单明了。但是，Rust里面实现一个类，就跟C++/Java非常不一样。

### OOP的实现工具

Rust中间实现面向对象的工具非常具有一致性和简洁性（跟C++比起来貌似孱弱）。好处：没什么好选的，就只有大概一种方法来实现。坏处：不够魔法。

#### 封装

从语法结构上来，封装信息和方法，Rust使用`struct`和`impl`。

对于前面那个例子，Rust的源代码`string.rs`中包括了如下实现：

```rust
pub struct String {
    vec: Vec<u8>,
}

// Methods from Deref<Target = str>
impl Deref for String {
    type Target = str;
    fn deref(&self) -> &str {
        unsafe { str::from_utf8_unchecked(&self.vec) }
    }
}

impl str{
    pub fn trim(&self) -> &str {
        self.trim_matches(|c: char| c.is_whitespace())
    }

}

// impl pub methods for String

impl String{
        pub fn replace<P: Pattern>(&self, from: P, to: &str) -> String {
        // Fast path for replacing a single ASCII character with another.
        if let Some(from_byte) = match from.as_utf8_pattern() {
            Some(Utf8Pattern::StringPattern([from_byte])) => Some(*from_byte),
            Some(Utf8Pattern::CharPattern(c)) => c.as_ascii().map(|ascii_char| ascii_char.to_u8()),
            _ => None,
        } {
            if let [to_byte] = to.as_bytes() {
                return unsafe { replace_ascii(self.as_bytes(), from_byte, *to_byte) };
            }
        }
        // Set result capacity to self.len() when from.len() <= to.len()
        let default_capacity = match from.as_utf8_pattern() {
            Some(Utf8Pattern::StringPattern(s)) if s.len() <= to.len() => self.len(),
            Some(Utf8Pattern::CharPattern(c)) if c.len_utf8() <= to.len() => self.len(),
            _ => 0,
        };
        let mut result = String::with_capacity(default_capacity);
        let mut last_end = 0;
        for (start, part) in self.match_indices(from) {
            result.push_str(unsafe { self.get_unchecked(last_end..start) });
            result.push_str(to);
            last_end = start + part.len();
        }
        result.push_str(unsafe { self.get_unchecked(last_end..self.len()) });
        result
    }

    pub fn to_uppercase(&self) -> String {
        // Fast path for replacing a single ASCII character with another.
        if let Some(from_byte) = match from.as_utf8_pattern() {
            Some(Utf8Pattern::StringPattern([from_byte])) => Some(*from_byte),
            Some(Utf8Pattern::CharPattern(c)) => c.as_ascii().map(|ascii_char| ascii_char.to_u8()),
            _ => None,
        } {
            if let [to_byte] = to.as_bytes() {
                return unsafe { replace_ascii(self.as_bytes(), from_byte, *to_byte) };
            }
        }
        // Set result capacity to self.len() when from.len() <= to.len()
        let default_capacity = match from.as_utf8_pattern() {
            Some(Utf8Pattern::StringPattern(s)) if s.len() <= to.len() => self.len(),
            Some(Utf8Pattern::CharPattern(c)) if c.len_utf8() <= to.len() => self.len(),
            _ => 0,
        };
        let mut result = String::with_capacity(default_capacity);
        let mut last_end = 0;
        for (start, part) in self.match_indices(from) {
            result.push_str(unsafe { self.get_unchecked(last_end..start) });
            result.push_str(to);
            last_end = start + part.len();
        }
        result.push_str(unsafe { self.get_unchecked(last_end..self.len()) });
        result
    }
}


// impl trait From<&str> for String
impl From<&str> for String {
    #[inline]
    fn from(s: &str) -> String {
        s.to_owned()
    }
}   
```

这些代码都是用Rust的源代码中摘录的，Rust就是这一点好，可以直接从源代码学习。[String帮助](https://doc.rust-lang.org/std/string/struct.String.html#)中直接可以点到各个方法的实现源文件。

上面的代码给出了Rust封装数据的唯一方法，一个`struct`。而方法的情况就有一点点复杂。

- `Deref`成别的类型，并获得相应的方法
- `impl`方法，直接定义新的方法
- `impl`一个`trait`，定义该`trait`的实现

Rust中的数据和方法都采用`pub`关键字来控制可见性。

#### 继承与多态

Rust并没有直接的继承，但是有`trait`来实现接口类似的多态行为和`is-a`关系。

```rust
{{% codeseg "static/rust/oop-in-rust/src/main.rs" 1 23 %}}
```

上面的代码构成了两个类，`Dog`和`Cat`，都实现了`Animal`这个`trait`，所以可以被当作`Animal`来使用；并且通过`dyn`关键字，可以实现多态行为。

```rust
{{% codeseg "static/rust/oop-in-rust/src/main.rs" 26 33 %}}
```

#### ADT的实现

当然，Rust还提供了`enum`来实现ADT。见：[ADT in Rust](/posts/rust/adt-first-thought)。

### 如何创建可以工作的类

这玩意是一种艺术、也是一种技术。可能大概似乎能写一本书吧……

大概来说呢，ADT是一个比较好的开头，通过ADT对领域的概念、对象、状态、行为进行大概的建模（数据和方法）。然后在ADT的基础上，增加更多的抽象层次（`trait`），来构造良好的抽象、更好的封装（对概念的封装）。

一般而言，要避免使用完能类（什么都装导致一个类失去了核心的概念，导致不能继续修仙等级不得寸进）。

第二个就是仔细审核不包含行为（或者很勉强包含很少的非关键行为）的类是否有必要存在？是否应该作为别的类的属性？

第三个就是好好查看那些用动词命名的类，是否应该只是一个`trait`?这种类的数据，有些时候就应该作为方法的参数而存在。

当然，这个工作的难点也就在这里：上面所有的原则都没有什么强制性，也很难有绝对正确，实际上连证据都很难认定。最终，通常都被归结为风格……

### 总结

OOP的难点在于审美和风格之类的玩意我们秃子玩不来。
