package com.alan344.constants;

import com.alan344.bean.config.ExtraTemplateFileConfig;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author AlanSun
 * @date 2022/11/21 16:01
 */
public class ConfigConstants {
    /**
     * 当前的额外文件配置
     */
    public static List<ExtraTemplateFileConfig> extraTemplateFileConfigs;

    public static Map<String, String> internalGlobalParam = new HashMap<>();
}
