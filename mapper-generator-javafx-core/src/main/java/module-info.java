open module com.alan344 {
    requires static lombok;
    requires mybatis.extension.plugin;

    requires org.mybatis.generator;
    requires com.github.javaparser.core;
    requires org.apache.commons.io;
    requires java.annotation;

    requires spring.context;
    requires spring.beans;

    requires javafx.controls;
    requires javafx.fxml;
    requires com.alibaba.fastjson2;
    requires com.zaxxer.hikari;
    requires org.kordamp.ikonli.javafx;
    requires java.sql;
    requires java.desktop;
    requires org.kordamp.ikonli.unicons;

    exports com.alan344;
}