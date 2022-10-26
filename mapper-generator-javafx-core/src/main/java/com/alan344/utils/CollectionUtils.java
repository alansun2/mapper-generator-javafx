package com.alan344.utils;

import java.util.Collection;

/**
 * @author AlanSun
 * @date 2022/10/25 22:41
 */
public class CollectionUtils {

    public static boolean isNotEmpty(Collection<?> collection) {
        return null != collection && !collection.isEmpty();
    }


    public static boolean isEmpty(Collection<?> collection) {
        return null == collection || collection.isEmpty();
    }
}
