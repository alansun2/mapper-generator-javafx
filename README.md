# mapper-generator-javafx
![](https://img.shields.io/badge/JavaFx-8-green.svg) 
![](https://img.shields.io/badge/SpringBoot-2.1.5-blue.svg)
![](https://img.shields.io/badge/version-1.0.0-orange.svg)
[![](https://img.shields.io/badge/downloads-1.0.0-brightgreen)](https://github.com/alansun2/mapper-generator-javafx/releases)

这是一个由 `JavaFX`，`SpringBoot` 开发的 mybatis-mapper-generator 小工具。可以让你快速生成数据库对应的实体类以及 Mybatis Mapper。目前本工具只支持 `MySQL`和与`MySQL`兼容的数据库(例如，`MariaDB`，`POLARDB`等)。如果有需求可以在 issue 上提，谢谢！

该工具依赖`mybatis-generator`（mybatis 官方自动生成工具）。

功能：
* 可视化生成数据库相应的实体类，不用再写配置文件

* **能够 merge 之前的 java bean 和 xml**
   比如你在使用了 `mapper-generator-javafx` 生成的 bean 上新增了一个方法或property，当你使用 **merge** 功能时新增的方法或 property 会保留。xml 同理。

这个 java bean ***merge* 功能官方并没有实现，官方的xml **merge** 也不是很好。

* 记录你每一次的变动
   这里讲一下自身使用官方的 `mybatis-generator` 感受，当我有多个数据源时，刚开始我在`数据源1`工作，生成`OrderInfo`，`OrderInfoMapper`，`OrderInfoMapper.xml`三个文件（忽略了一些字段，去除了delete update sql），接着我又在`数据源2`工作，此时我已经把`数据源1`的配置删除，后来我对`数据源1`的`order_info`表回复一些之前忽略的字段，或者我要去除一个 count sql方法，这时我又要重新配置`数据源1`，并且要比对之前生成的文件，很是麻烦。也许你会说，使用多个配置文件来回切换就可以。当然这也是一种方法。不过我相信当你用了`mybatis-friend`，你应该会抛弃这种想法。
   
### 1. 使用步骤介绍
1. 右上角菜单栏点击文件 -> 添加数据源

    ![adddatasource.png](https://upload-images.jianshu.io/upload_images/5614480-72f0e85b3f8ddd97.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
2. 展开数据源，对需要的表进行导出（右键导出），也可以选择数据源导出该数据源所有表

    点击数据源导出会导出数据源下所有表
    
    ![datasource-rightckick.png](https://upload-images.jianshu.io/upload_images/5614480-b7c3abc0a51994ce.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
    
    点击表导出(可多选)
    
    ![table-rightclick.png](https://upload-images.jianshu.io/upload_images/5614480-a58f0d256fd70c1a.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

3. 对需要导出的表进行配置
    表配置详情
    
    ![table-detail.png](https://upload-images.jianshu.io/upload_images/5614480-8c874dd5e9d45c1a.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
    
    表字段详情
    
    ![table-column-detail.png](https://upload-images.jianshu.io/upload_images/5614480-c6f4062b785870d8.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

4. 配置完成后点击右上角导出按钮，对导出位置进行配置
    配置导出位置
    
    ![export.png](https://upload-images.jianshu.io/upload_images/5614480-fede595824c044da.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)


5. 点击应用

### 2. 运行环境
java8， classpath 必须要有 javafx 相关的 jar (一般如果你是安装的 oracle 官网的 jdk，不会有问题)。

### 3. 数据目录
数据目录放在当前用户的 `/AppData/Local/MapperGenerator/data` 下

### 4. 配置目录
导出配置存放在 前用户的 `/AppData/Local/MapperGenerator/config` 下

---

**如果工具发生错误，可以试一下清空以上目录。如果还不行，欢迎在 github 上题issue。**

---

### 5. 日志文件位置
日志文件位置：
* windows-exe 版本: 安装目录下 `app/mybatis-friend.log`
* jar 包版本：jar的同级目录`mybatis-friend.log`

### 6. 如何自定义开发自己的功能？
如果你要在此基础上开发自己的功能，请先 pull [mybatis-generator](https://github.com/alansun2/generator)。因为 `mapper-generator-javafx` 项目依赖 `mybatis-generator`。

`mybatis-generator` 是 fork 官方的一个用于生成 mapper 的插件。本人对该插件做了一定的自定义。所以如果你要自定义 `mapper-generator-javafx` 必须先 `pull mybatis-generator` 并打包，否则源码会报错。

该项目大概4000多行代码，很简单的代码。但你得有`JavaFX`的基础，这里给 [B 站的可爱阿婆主《JavaFX没人看系列》]([https://space.bilibili.com/5096022/video?tid=36&page=8&keyword=&order=pubdate](https://space.bilibili.com/5096022/video?tid=36&page=8&keyword=&order=pubdate) 做一波免费广告（哈哈哈）。

### 结束
附上 github 地址和下载地址，下载包含两个版本：
* `mybatis-friend-windows-exe.zip`：可以直接运行在 `windows`
* `mybatis-friend-executable.jar`：可执行 jar 

> 本项目 github 源码地址：https://github.com/alansun2/mapper-generator-javafx
> 本项目[下载地址](https://github.com/alansun2/mapper-generator-javafx/releases)