package com.alan344.constants.enums;

import com.alan344.mybatis.IntrospectedTableMybatisFlexImpl;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author AlanSun
 * @date 2023/3/28 23:54
 **/
@Getter
@AllArgsConstructor
public enum TargetNameEnum {
    /**
     * targetName
     */
    Mybatis3("Mybatis3"),
    MyBatis3Simple("MyBatis3Simple"),
    MyBatis3Flex(IntrospectedTableMybatisFlexImpl.class.getName()),
    MyBatis3DynamicSql("MyBatis3DynamicSql"),
    MyBatis3Kotlin("MyBatis3Kotlin");

    private final String value;
}
