# mapper-generator-javafx

![](https://img.shields.io/badge/JavaFx-8-green.svg)
![](https://img.shields.io/badge/SpringBoot-2.1.5-blue.svg)
![](https://img.shields.io/badge/version-4.0.0-orange.svg)
[![](https://img.shields.io/badge/downloads-4.0.0-brightgreen)](https://github.com/alansun2/mapper-generator-javafx/releases)

这是一个由 `JavaFX`，`SpringBoot` 开发的 mybatis-mapper-generator 小工具。可以让你快速生成数据库对应的实体类以及 Mybatis
Mapper。目前本工具只支持 `MySQL`和与`MySQL`兼容的数据库(例如，`MariaDB`，`POLARDB`等)。如果有需求可以在 issue 上提，谢谢！

该工具依赖`mybatis-generator`（mybatis 官方自动生成工具）。

功能：

* 可视化生成数据库相应的实体类，不用再写配置文件

* 记录你每一次的变动
  这里讲一下自身使用官方的 `mybatis-generator` 感受，当我有多个数据源时，刚开始我在`数据源1`工作，生成`OrderInfo`
  ，`OrderInfoMapper`，`OrderInfoMapper.xml`三个文件（忽略了一些字段，去除了delete update sql），接着我又在`数据源2`
  工作，此时我已经把`数据源1`的配置删除，后来我对`数据源1`的`order_info`表回复一些之前忽略的字段，或者我要去除一个 count
  sql方法，这时我又要重新配置`数据源1`
  ，并且要比对之前生成的文件，很是麻烦。也许你会说，使用多个配置文件来回切换就可以。当然这也是一种方法。不过我相信当你用了`mybatis-friend`
  ，你应该会抛弃这种想法。

### VERSION UPDATE

* v4.0.0
    1. 重构代码，优化UI
    2. 新增模板功能
* v2.1.0
    1. 增加 tk.mybatis 的生成策略
    2. 增加 mybatis 生成策略（MyBatis3Simple，MyBatis3DynamicSql）
    3. 优化代码
* v2.0.0
    1. 增加可选择的实现 Serializable 接口的功能
    2. 重构代码
    3. 优化搜索
    4. 修改一些bug
* 1.1.0
  增加表搜索功能，左边列表区域使用键盘输入就可以使用

### 1. 使用步骤介绍

1. 右上角菜单栏点击文件 -> 添加数据源

   ![adddatasource.jpg](https://upload-images.jianshu.io/upload_images/5614480-6a038858b17097a1.jpg?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

2. 展开数据源，对需要的表进行导出（右键导出），也可以选择数据源导出该数据源所有表

   点击数据源导出会导出数据源下所有表，数据源的刷新只是对表重新加载，并不会对字段重新加载。对字段的重新加载请看第三点

   ![datasource-rightckick.jpg](https://upload-images.jianshu.io/upload_images/5614480-2c51a393afe3f759.jpg?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

   点击表导出(可多选)

   ![table-rightclick.jpg](https://upload-images.jianshu.io/upload_images/5614480-6b39547b69601898.jpg?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

3. 对需要导出的表进行配置

   表配置，对想要导出的 sql 打上勾

   ![table-detail.jpg](https://upload-images.jianshu.io/upload_images/5614480-05f12a8b5f141bb9.jpg?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

   表字段配置，可以忽略导出字段，指定导出的属性名(property)，对属性名的类型进行重写(java type)，对属性配置类型处理器（type
   handler）

   ![table-column-detail.jpg](https://upload-images.jianshu.io/upload_images/5614480-be5a457502b2ef8a.jpg?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

   表字段刷新

   ![column-refresh.jpg](https://upload-images.jianshu.io/upload_images/5614480-c57eb2ca36b3710e.jpg?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

4. 配置完成后点击右上角导出按钮，对导出位置进行配置
   配置导出位置

   ![export.jpg](https://upload-images.jianshu.io/upload_images/5614480-cda7b8fb39294f2a.jpg?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

5. 点击应用

### 2. 运行环境

java8， classpath 必须要有 javafx 相关的 jar (一般如果你是安装的 oracle 官网的 jdk，不会有问题)。

### 3. 数据目录

数据目录放在当前用户的 `/AppData/Local/MapperGeneratorV2/data` 下

### 4. 配置目录

导出配置存放在 前用户的 `/AppData/Local/MapperGeneratorV2/config` 下

---

**如果工具发生错误，可以试一下清空以上目录。如果还不行，欢迎在 github 上题issue。**

---

### 5. 日志文件位置

日志文件位置：

* windows-exe 版本: 安装目录下 `app/mybatis-friend.log`
* jar 包版本：jar的同级目录`mybatis-friend.log`

### 6. 如何自定义开发自己的功能？

该项目大概4000多行代码，很简单的代码。但你得有`JavaFX`
的基础，这里给 [B 站的可爱阿婆主《JavaFX没人看系列》](https://space.bilibili.com/5096022/video?tid=36&page=8&keyword=&order=pubdate)
做一波广告（哈哈哈）。

#### 6.1 使用 jlink 生成项目的 jre

1. mvn clean package 生成 lib
2. 运行一下命令

```shell
jlink --module-path lib --add-modules java.base,java.sql,javafx.controls,javafx.fxml,javafx.graphics,java.naming,org.kordamp.ikonli.unicons --output image
```

### 结束

附上 github 地址和下载地址，下载包含两个版本：

* `mybatis-friend-windows-exe.7z`：可以直接运行在 `windows`
* `mybatis-friend-executable.jar`：可执行 jar

> 本项目 github 源码地址：https://github.com/alansun2/mapper-generator-javafx
>
> 本项目 gitee 源码地址：https://gitee.com/alansc/mapper-generator-javafx
>
> 本项目git[下载地址](https://github.com/alansun2/mapper-generator-javafx/releases)
>
> 本项目gitee[下载地址](https://gitee.com/alansc/mapper-generator-javafx/releases)