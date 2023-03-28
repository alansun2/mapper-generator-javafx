package com.alan344.constants.enums;

import java.util.List;

/**
 * @author AlanSun
 * @date 2023/3/29 0:05
 **/
public enum JavaClientTypeEnum {
    /**
     * mapper
     */
    XMLMAPPER,
    MIXEDMAPPER,
    ANNOTATEDMAPPER;

    public static final List<JavaClientTypeEnum> JAVA_CLIENT_TYPE_ENUMS = List.of(XMLMAPPER, ANNOTATEDMAPPER);
}
