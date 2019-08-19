package com.alan344.bean;/**
 * @author ：AlanSun
 * @date ：2019/8/19 22:39
 * @description ：${description}
 * @since : $version$
 */

import lombok.Getter;
import lombok.Setter;

/**
 * @author AlanSun
 * @date 2019/8/19 22:39
 */
@Getter
@Setter
public class ColumnOverride {
    /**
     * column 对应的 bean 属性名
     */
    private String property;
    /**
     * 指定的 Java 类型
     */
    private String javaType;
    /**
     * 自定义类型解析器
     */
    private String typeHandler;
    /**
     * 是否总是生成，如果 = true, 则 insert， update  语句中就不加入该字段
     */
    private boolean isGeneratedAlways;
    /**
     * 是否是关键字
     */
    private boolean delimitedColumnName;
}
