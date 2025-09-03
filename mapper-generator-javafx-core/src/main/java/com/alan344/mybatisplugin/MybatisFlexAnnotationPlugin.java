package com.alan344.mybatisplugin;

import org.mybatis.generator.api.PluginAdapter;

import java.util.List;

/**
 * @author AlanSun
 * @since 2025/7/28 18:19
 */
public class MybatisFlexAnnotationPlugin extends PluginAdapter {
    @Override
    public boolean validate(final List<String> warnings) {
        return false;
    }
}
