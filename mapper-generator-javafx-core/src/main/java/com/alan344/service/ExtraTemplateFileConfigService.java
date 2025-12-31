package com.alan344.service;

import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.ExtraTemplateFileGroupConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.CollectionUtils;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONWriter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @since 2022/11/19 23:35
 */
@Slf4j
@Service
public class ExtraTemplateFileConfigService {
    @Value("classpath:default-extra-template-file-config.json")
    private Resource resource;

    private List<ExtraTemplateFileGroupConfig> extraTemplateFileGroupConfigs;

    public void saveExtraFileConfig(List<ExtraTemplateFileGroupConfig> items) {
        // 去除系统配置
        final List<ExtraTemplateFileGroupConfig> extraTemplateFileGroupConfigs = items.stream()
                .filter(extraTemplateFileGroupConfig -> !extraTemplateFileGroupConfig.isSystem())
                .collect(Collectors.toList());
        try {
            FileUtils.writeStringToFile(BaseConstants.getExtraFileConfigFile(),
                    JSONArray.toJSONString(extraTemplateFileGroupConfigs, JSONWriter.Feature.PrettyFormat,
                            JSONWriter.Feature.WriteEnumsUsingName), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public Map<String, ExtraTemplateFileConfig> getExtraFileConfigMap(List<String> templateIds) {
        if (CollectionUtils.isEmpty(templateIds)) {
            return Collections.emptyMap();
        }
        final List<ExtraTemplateFileGroupConfig> extraTemplateFileConfigList = this.getExtraTemplateFileGroupConfig();
        if (CollectionUtils.isEmpty(extraTemplateFileConfigList)) {
            return Collections.emptyMap();
        }


        final Set<String> templateIdSet = new HashSet<>(templateIds);
        return extraTemplateFileConfigList.stream().flatMap(extraTemplateFileGroupConfig -> extraTemplateFileGroupConfig.getList().stream())
                .filter(extraTemplateFileConfig -> templateIdSet.contains(extraTemplateFileConfig.getId()))
                .collect(Collectors.toMap(ExtraTemplateFileConfig::getId, Function.identity()));
    }

    public List<ExtraTemplateFileGroupConfig> getExtraTemplateFileGroupConfig() {
        if (null != extraTemplateFileGroupConfigs) {
            return extraTemplateFileGroupConfigs;
        }

        // 家在自定义的配置
        final File extraFileConfigFile = BaseConstants.getExtraFileConfigFile();
        if (!extraFileConfigFile.exists()) {
            extraTemplateFileGroupConfigs = new ArrayList<>();
        } else {
            try {
                extraTemplateFileGroupConfigs = JSONArray.parseArray(FileUtils.readFileToString(extraFileConfigFile,
                        StandardCharsets.UTF_8)).toList(ExtraTemplateFileGroupConfig.class);
                // 确保每个分组都有非空的配置列表
                extraTemplateFileGroupConfigs.forEach(extraTemplateFileGroupConfig -> {
                    Collection<ExtraTemplateFileConfig> configList =
                            extraTemplateFileGroupConfig.getExtraTemplateFileConfigList();
                    if (CollectionUtils.isEmpty(configList)) {
                        extraTemplateFileGroupConfig.setExtraTemplateFileConfigList(new ArrayList<>());
                    }
                });
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        // 加载默认的配置
        final List<ExtraTemplateFileGroupConfig> defaults = this.getDefault();
        if (CollectionUtils.isEmpty(extraTemplateFileGroupConfigs)) {
            extraTemplateFileGroupConfigs = defaults;
        } else {
            // 将默认配置插入到开头
            extraTemplateFileGroupConfigs.addAll(0, defaults);
        }

        // 设置反向引用
        extraTemplateFileGroupConfigs.forEach(extraTemplateFileGroupConfig -> {
            Collection<ExtraTemplateFileConfig> configList = extraTemplateFileGroupConfig.getExtraTemplateFileConfigList();
            if (CollectionUtils.isNotEmpty(configList)) {
                configList.forEach(extraTemplateFileConfig ->
                        extraTemplateFileConfig.setExtraTemplateFileGroupConfig(extraTemplateFileGroupConfig));
            }
        });
        return extraTemplateFileGroupConfigs;
    }

    /**
     * 获取默认分组
     *
     * @return 模板分组
     */
    private List<ExtraTemplateFileGroupConfig> getDefault() {
        try {
            final InputStream inputStream = resource.getInputStream();
            return JSON.parseArray(inputStream).toList(ExtraTemplateFileGroupConfig.class);
        } catch (IOException e) {
            log.error("获取默认模板分组失败", e);
        }

        return Collections.emptyList();
    }
}
