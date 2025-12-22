package com.alan344.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author AlanSun
 * @since 2025/12/23 01:24
 **/
@Getter
@AllArgsConstructor
public enum HttpParamType {
    NONE,
    PATH,
    QUERY,
    BODY
}