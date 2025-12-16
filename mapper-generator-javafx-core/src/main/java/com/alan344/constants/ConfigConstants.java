package com.alan344.constants;

import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.utils.tokenparse.GenericTokenParser;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author AlanSun
 * @since 2022/11/21 16:01
 */
public class ConfigConstants {
    /**
     * 当前的额外文件配置
     */
    public static List<ExtraTemplateFileConfig> extraTemplateFileConfigs;

    public static Map<String, String> namePackageMap = new LinkedHashMap<>();

    public static Map<String, String> globalParam = new HashMap<>();

    public static final GenericTokenParser GENERIC_TOKEN_PARSER = new GenericTokenParser("${", "}");
}
