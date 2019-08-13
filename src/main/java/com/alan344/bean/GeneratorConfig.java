package com.alan344.bean;

import lombok.Getter;
import lombok.Setter;

/**
 * @author AlanSun
 * @date 2019/8/13 16:20
 */
@Getter
@Setter
public class GeneratorConfig {
    /**
     * 配置的名称
     */
    private String configName;
    /**
     * 类中的作者信息
     */
    private String author;
    /**
     * 是否使用java8
     */
    private boolean userJava8 = true;
    /**
     * 是否使用追加
     */
    private boolean isAppend;
    /**
     * bean 包名
     */
    private String beanPackage;
    /**
     * mapperBean 包名
     */
    private String mapperBeanPackage;
    /**
     * java地址
     */
    private String exportAddress;
    /***
     * xml导出地址
     */
    private String mapperXmlAddress;
}
