# mapper-generator-javafx
这是一个由 `javafx`，`springboot` 开发的 mybatis-mapper-generator 小工具。
让你可以快速生成数据库对应的实体类。目前只持支 `mysql`。如果有需求可以在 issue 上提，谢谢！

功能：
* 可视化生成数据库相应的实体类，不用再写配置文件
* 能够 **merge** 之前的 java bean 和 xml
   
   比如你在使用了 `mapper-generator-javafx` 生成的 bean 上新增了一个方法或 property，当你
   使用 **merge** 功能时新增的方法或 property 会保留。xml 同理
* 记录你每一次的变动

    这里讲一下自身使用官方的 mybatis-generator 感受，当我有多个数据源时，刚开始我在*数据源1*工作，生成了
    `OrderInfo`，`OrderInfoMapper`，`OrderInfoMapper.xml`三个文件，接着我又在`数据源2`工作，此时我已经把*数据源1*的配置删除，
    后来我对数据源的`order_info`进行了修改，或者我要去除一个delete方法。这时我又要重新配置*数据源1*。很是麻烦，也许你会说，使用
    多个配置文件来回切换就可以。当然这也是一种方法。

### 1. 运行环境
java8， classpath 必须要有 javafx 相关的 jar (一般如果你是安装的 oracle 官网的 jdk，不会有问题)

### 2. 数据目录
数据目录放在当前用户的 `/AppData/Local/MapperGenerator/data` 下

### 3. 配置目录
导出配置存放在 前用户的 `/AppData/Local/MapperGenerator/config` 下

### 4. 日志文件位置
日志文件在安装目录下 `app/mybatis-friend.log`

如果工具发生错误，可以试一下清空以上目录。如果还不行，欢迎在 github 上题issue。

就是这样。最后附上 github 地址和下载地址，release 会包含两个版本：

* `mybatis-friend-windows-exe.zip`：可以直接运行在 `windows`
* `mybatis-friend-executable.jar`: 可执行 jar 

> github 源码地址：https://github.com/alansun2/mapper-generator-javafx

[下载地址](https://github.com/alansun2/mapper-generator-javafx/releases)

### 4. 如何在此基础上自定义开发自己的功能？
如果你要在此基础上开发自己的功能，请先 pull [mybatis-generator](https://github.com/alansun2/generator)
。因为 `mapper-generator-javafx` 项目依赖 `mybatis-generator`。

mybatis-generator 是 fork 官方的一个用于生成 mapper 的插件。本人对该插件做了一定的自定义。所以
如果你要自定义 `mapper-generator-javafx` 必须先 pull mybatis-generator 并打包。
