package com.alan344.service;

import cn.hutool.core.collection.CollectionUtil;
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
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2022/11/19 23:35
 */
@Slf4j
@Service
public class ExtraTemplateFileConfigService {
    @Value("classpath:default-extra-template-file-config.json")
    private Resource resource;

    private List<ExtraTemplateFileGroupConfig> extraTemplateFileConfigs;

    public void saveExtraFileConfig(List<ExtraTemplateFileGroupConfig> items) {
        // 去除系统配置
        final List<ExtraTemplateFileGroupConfig> extraTemplateFileGroupConfigs = items.stream()
                .filter(extraTemplateFileGroupConfig -> !extraTemplateFileGroupConfig.isSystem())
                .collect(Collectors.toList());

        try {
            FileUtils.writeStringToFile(BaseConstants.getExtraFileConfigFile(), JSONArray.toJSONString(extraTemplateFileGroupConfigs, JSONWriter.Feature.PrettyFormat, JSONWriter.Feature.WriteEnumsUsingName), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public Map<String, ExtraTemplateFileConfig> getExtraFileConfigMap(List<String> templateIds) {
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
        if (null != extraTemplateFileConfigs) {
            return extraTemplateFileConfigs;
        }

        final File extraFileConfigFile = BaseConstants.getExtraFileConfigFile();
        if (!extraFileConfigFile.exists()) {
            extraTemplateFileConfigs = new ArrayList<>();
        } else {
            try {
                extraTemplateFileConfigs = JSONArray.parseArray(FileUtils.readFileToString(extraFileConfigFile, StandardCharsets.UTF_8)).toList(ExtraTemplateFileGroupConfig.class);
                extraTemplateFileConfigs.forEach(extraTemplateFileGroupConfig -> {
                    // 防止空指针
                    final Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList = extraTemplateFileGroupConfig.getExtraTemplateFileConfigList();
                    if (CollectionUtil.isEmpty(extraTemplateFileConfigList)) {
                        extraTemplateFileGroupConfig.setExtraTemplateFileConfigList(new ArrayList<>());
                    }
                });
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        final List<ExtraTemplateFileGroupConfig> defaults = this.getDefault();
        if (CollectionUtils.isEmpty(extraTemplateFileConfigs)) {
            extraTemplateFileConfigs = defaults;
        } else {
            for (int i = 0; i < defaults.size(); i++) {
                extraTemplateFileConfigs.add(i, defaults.get(i));
            }
        }
        return extraTemplateFileConfigs;
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
