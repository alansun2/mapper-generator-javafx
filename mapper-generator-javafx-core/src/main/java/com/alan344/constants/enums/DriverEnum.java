package com.alan344.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2020/9/30 10:43
 */
@Getter
@AllArgsConstructor
public enum DriverEnum {

    /**
     * mysql 8.0.16 版本
     */
    MYSQL_8("com.mysql.cj.jdbc.Driver", "jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=utf-8&useSSL=false&serverTimezone=GMT%2B8"),
    /**
     * oracle 11g
     */
    ORACLE_11("oracle.jdbc.OracleDriver", "jdbc:oracle:thin:@localhost:1521:orcl"),
    ;

    private final String driveName;

    private final String url;

    public final static Set<String> DRIVER_NAMES = Arrays.stream(DriverEnum.values()).map(DriverEnum::getDriveName).collect(Collectors.toSet());
    public final static Set<String> URLS = Arrays.stream(DriverEnum.values()).map(DriverEnum::getUrl).collect(Collectors.toSet());
}
