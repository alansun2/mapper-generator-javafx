package com.alan344.utils;

import org.springframework.util.StringUtils;

/**
 * @author AlanSun
 * @date 2023/1/24 0:02
 */
public class EnvUtils {
    public static Env getEnv() {
        final String env = System.getProperty("env");
        if (StringUtils.hasText(env) && Env.valueOf(env.toUpperCase()) == Env.PRO) {
            return Env.PRO;
        }
        return Env.DEV;
    }

    public enum Env {
        /**
         * pro
         */
        PRO,
        DEV
    }
}
