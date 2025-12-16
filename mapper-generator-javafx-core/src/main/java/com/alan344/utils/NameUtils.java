package com.alan344.utils;

import cn.hutool.core.util.RandomUtil;

import java.util.Collection;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @since 2023/6/10 19:52
 */
public class NameUtils {

    /**
     * 生成名称, 如果名称重复, 则在名称后面加上-COPY, 如果还重复, 则在后面加上随机数
     *
     * @param name       原始名称
     * @param existNames 已存在的名称
     * @return 生成的名称
     */
    public static String generatorName(String name, Collection<? extends CheckNameRepeat> existNames) {
        final Set<String> existNameSet = existNames.stream().map(CheckNameRepeat::getName).filter(Objects::nonNull).collect(Collectors.toSet());

        StringBuilder stringBuilder = new StringBuilder(name);

        stringBuilder.append("-").append("COPY");
        while (existNameSet.contains(stringBuilder.toString())) {
            stringBuilder.append("-").append(RandomUtil.randomLong(1, 999999999));
        }
        return stringBuilder.toString();
    }

    public interface CheckNameRepeat {
        String getName();
    }
}
