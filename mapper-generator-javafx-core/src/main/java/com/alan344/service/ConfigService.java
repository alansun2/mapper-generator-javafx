package com.alan344.service;

import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.BeanUtils;
import com.alan344.utils.CollectionUtils;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONWriter;
import lombok.Getter;
import lombok.Setter;
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
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2019/8/15 18:10
 */
@Slf4j
@Service
public class ConfigService {
    @Value(value = "classpath:default-extra-file-config.json")
    private Resource resource;
    /**
     * 配置信息 map
     */
    @Getter
    @Setter
    private Map<String, MybatisExportConfig> configNameConfigMap = new HashMap<>();

    private LinkedList<MybatisExportConfig> mybatisExportConfigs2 = new LinkedList<>();

    private boolean isLoaded;

    /**
     * 添加配置
     *
     * @param mybatisExportConfig 配置信息
     */
    public void addConfig(MybatisExportConfig mybatisExportConfig) {
        LinkedList<MybatisExportConfig> mybatisExportConfigs = this.loadConfigFromFile();

        LinkedList<MybatisExportConfig> existConfigLinkedList = mybatisExportConfigs.stream()
                .filter(generatorConfig1 -> mybatisExportConfig.getConfigName().equals(generatorConfig1.getConfigName()))
                .collect(LinkedList::new, LinkedList::add, List::addAll);

        // 配置已存在，如果内容修改，则修改
        if (!existConfigLinkedList.isEmpty()) {
            MybatisExportConfig olderConfig = existConfigLinkedList.getFirst();
            boolean isSame = BeanUtils.checkPropertyOfBean(mybatisExportConfig, olderConfig);
            if (!isSame) {
                // 原来文件被修改
                final int i = mybatisExportConfigs.indexOf(olderConfig);
                mybatisExportConfigs.remove(olderConfig);
                mybatisExportConfigs.add(i, mybatisExportConfig);
                this.saveConfigToFile();
                this.configNameConfigMap.put(mybatisExportConfig.getConfigName(), mybatisExportConfig);
            }
        } else {
            // 新配置
            mybatisExportConfigs.addFirst(mybatisExportConfig);
            this.saveConfigToFile();
            this.configNameConfigMap.put(mybatisExportConfig.getConfigName(), mybatisExportConfig);
        }
    }

    /**
     * 把配置写入文件
     */
    public void saveConfigToFile() {
        final LinkedList<MybatisExportConfig> mybatisExportConfigs = this.loadConfigFromFile();
        if (CollectionUtils.isEmpty(mybatisExportConfigs)) {
            return;
        }
        final List<MybatisExportConfig> mybatisExportConfigsClone = mybatisExportConfigs.stream().map(MybatisExportConfig::clone).toList();
        // 删除内置的配置
        mybatisExportConfigsClone.forEach(mybatisExportConfig -> {
            final List<ExtraFileGroupConfig> extraFileGroupConfigs = mybatisExportConfig.getExtraFileGroupConfigs();
            if (CollectionUtils.isNotEmpty(extraFileGroupConfigs)) {
                extraFileGroupConfigs.removeIf(ExtraFileGroupConfig::isSystem);
            }
        });
        String configsStr = JSONArray.toJSONString(mybatisExportConfigsClone, JSONWriter.Feature.PrettyFormat, JSONWriter.Feature.WriteEnumsUsingName);
        try {
            FileUtils.writeStringToFile(BaseConstants.getBaseConfigFile(), configsStr, StandardCharsets.UTF_8.toString());
        } catch (IOException e) {
            log.error("写入配置信息失败", e);
        }
    }

    /**
     * 从文件加载配置至pane
     */
    public LinkedList<MybatisExportConfig> loadConfigFromFile() {
        File file = BaseConstants.getBaseConfigFile();
        if (!file.exists()) {
            return mybatisExportConfigs2;
        }

        if (isLoaded) {
            return mybatisExportConfigs2;
        } else {
            try {
                List<MybatisExportConfig> mybatisExportConfigs1 = JSON.parseArray(FileUtils.openInputStream(file)).toList(MybatisExportConfig.class);
                mybatisExportConfigs2 = mybatisExportConfigs1.stream().collect(LinkedList::new, LinkedList::add, List::addAll);
                isLoaded = true;
                configNameConfigMap = mybatisExportConfigs2.stream().collect(Collectors.toMap(MybatisExportConfig::getConfigName, o -> o));
                return mybatisExportConfigs2;
            } catch (IOException e) {
                log.error("加载dataSource文件失败", e);
                return mybatisExportConfigs2;
            }
        }
    }

    /**
     * 获取额外配置
     *
     * @return {@link ExtraFileGroupConfig}s
     */
    public List<ExtraFileGroupConfig> getExtraFileGroupConfigs(MybatisExportConfig mybatisExportConfig) {
        List<ExtraFileGroupConfig> extraFileGroupConfigs = mybatisExportConfig.getExtraFileGroupConfigs();
        final List<ExtraFileGroupConfig> defaults = this.getDefaults(mybatisExportConfig);
        if (CollectionUtils.isEmpty(extraFileGroupConfigs)) {
            extraFileGroupConfigs = defaults;
        } else {
            for (int i = 0; i < defaults.size(); i++) {
                extraFileGroupConfigs.add(i, defaults.get(i));
            }
        }

        return extraFileGroupConfigs;
    }

    private List<ExtraFileGroupConfig> getDefaults(MybatisExportConfig mybatisExportConfig) {
        try {
            final InputStream inputStream = resource.getInputStream();
            final List<ExtraFileGroupConfig> extraFileGroupConfigs = JSON.parseArray(inputStream).toList(ExtraFileGroupConfig.class);
            // 设置内置示例的导出地址
            extraFileGroupConfigs.forEach(extraFileGroupConfig -> {
                final Collection<ExtraFileGroupConfig.ExtraFileConfig> list = extraFileGroupConfig.getList();
                list.forEach(extraFileConfig -> {
                    StringBuilder sb = new StringBuilder();
                    if (extraFileConfig.getOutputPath().startsWith("-")) {
                        sb.append(mybatisExportConfig.getProjectName());
                    }
                    extraFileConfig.setOutputPath(sb + extraFileConfig.getOutputPath());
                });
            });
            return extraFileGroupConfigs;
        } catch (IOException e) {
            log.error("获取默认分组失败", e);
        }

        return Collections.emptyList();
    }
}
