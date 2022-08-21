package com.alan344.bean;

/**
 * @author AlanSun
 * @date 2020/12/3 14:42
 */
public class MybatisConfigThreadLocal {
    private static ThreadLocal<MybatisExportConfig> mybatisExportConfigThreadLocal = new ThreadLocal<>();

    public static void setMybatisExportConfig(MybatisExportConfig mybatisExportConfig) {
        mybatisExportConfigThreadLocal.set(mybatisExportConfig);
    }

    public static MybatisExportConfig getMybatisExportConfig() {
        return mybatisExportConfigThreadLocal.get();
    }

    public static void remove() {
        mybatisExportConfigThreadLocal.remove();
    }
}
