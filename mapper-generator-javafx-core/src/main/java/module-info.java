open module mybatis.friend {
    requires static lombok;
    requires spring.context;
    requires happyframework.core;
    requires javafx.controls;
    requires javafx.fxml;
    requires java.sql;
    requires org.mybatis.generator;
    requires com.github.javaparser.core;
    requires org.apache.commons.lang3;
    requires commons.io;
    requires fastjson;
    requires com.google.common;
    requires spring.beans;
    requires spring.jdbc;
    requires java.annotation;
    requires org.apache.commons.collections4;
    requires org.kordamp.ikonli.javafx;
    requires com.zaxxer.hikari;
    requires mybatis.extension.plugin;

//    opens com.alan344 to javafx.fxml;
//    opens com.alan344.controller to spring.beans;
    exports com.alan344;
}