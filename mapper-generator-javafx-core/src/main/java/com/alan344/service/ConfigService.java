package com.alan344.service;

import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.BeanUtils;
import com.alan344.utils.CollectionUtils;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONWriter;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author AlanSun
 * @date 2019/8/15 18:10
 */
@Slf4j
@Service
public class ConfigService {
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

        LinkedList<MybatisExportConfig> existConfigLinkedList = mybatisExportConfigs.stream().filter(generatorConfig1 -> mybatisExportConfig.getConfigName().equals(generatorConfig1.getConfigName()))
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
                this.saveConfigToFile(mybatisExportConfigs);
                this.configNameConfigMap.put(mybatisExportConfig.getConfigName(), mybatisExportConfig);
            }
        } else {
            // 新配置
            mybatisExportConfigs.addFirst(mybatisExportConfig);
            this.saveConfigToFile(mybatisExportConfigs);
            this.configNameConfigMap.put(mybatisExportConfig.getConfigName(), mybatisExportConfig);
        }
    }

    /**
     * 删除配置
     *
     * @param mybatisExportConfig 配置信息
     */
    public void deleteConfig(MybatisExportConfig mybatisExportConfig) {
        List<MybatisExportConfig> mybatisExportConfigs = this.loadConfigFromFile();
        mybatisExportConfigs.remove(mybatisExportConfig);
        this.saveConfigToFile(mybatisExportConfigs);
        this.configNameConfigMap.remove(mybatisExportConfig.getConfigName());
    }

    /**
     * 把配置写入文件
     *
     * @param mybatisExportConfigs 配置信息
     */
    private void saveConfigToFile(List<MybatisExportConfig> mybatisExportConfigs) {
        if (CollectionUtils.isEmpty(mybatisExportConfigs)) {
            return;
        }
        // 删除内置的配置
        mybatisExportConfigs.forEach(mybatisExportConfig -> {
            final List<ExtraFileGroupConfig> extraFileGroupConfigs = mybatisExportConfig.getExtraFileGroupConfigs();
            extraFileGroupConfigs.removeIf(ExtraFileGroupConfig::getIsSystem);
        });
        String configsStr = JSONArray.toJSONString(mybatisExportConfigs, JSONWriter.Feature.PrettyFormat, JSONWriter.Feature.WriteEnumsUsingName);
        try {
            FileUtils.writeStringToFile(BaseConstants.getConfigFile(), configsStr, StandardCharsets.UTF_8.toString());
        } catch (IOException e) {
            log.error("写入配置信息失败", e);
        }
    }

    /**
     * 从文件加载配置至pane
     */
    public LinkedList<MybatisExportConfig> loadConfigFromFile() {
        File file = BaseConstants.getConfigFile();
        if (!file.exists()) {
            return mybatisExportConfigs2;
        }

        if (isLoaded) {
            return mybatisExportConfigs2;
        } else {
            try {
                List<MybatisExportConfig> mybatisExportConfigs1 = JSONArray.parseArray(FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString())).toList(MybatisExportConfig.class);
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
    public List<ExtraFileGroupConfig> getExtraFileGroupConfigs() {
        final MybatisExportConfig currentConfig = BaseConstants.currentConfig;
        List<ExtraFileGroupConfig> extraFileGroupConfigs = currentConfig.getExtraFileGroupConfigs();
        final List<ExtraFileGroupConfig> defaults = this.getDefaults();
        if (CollectionUtils.isEmpty(extraFileGroupConfigs)) {
            extraFileGroupConfigs = defaults;
        } else {
            for (int i = 0; i < defaults.size(); i++) {
                extraFileGroupConfigs.add(i, defaults.get(i));
            }
        }

        return extraFileGroupConfigs;
    }

    private List<ExtraFileGroupConfig> getDefaults() {
        List<ExtraFileGroupConfig> extraFileGroupConfigs = new ArrayList<>();
        ExtraFileGroupConfig extraFileGroupConfig1 = new ExtraFileGroupConfig();
        extraFileGroupConfig1.setEnable(false);
        extraFileGroupConfig1.setIsSystem(true);
        extraFileGroupConfig1.setGroupName("usual test");
        final Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigSet1 = Stream.of("Controller",
                "ServiceI", "ServiceImpl").map(s -> {
            ExtraFileGroupConfig.ExtraFileConfig extraFileConfig = new ExtraFileGroupConfig.ExtraFileConfig();
            extraFileConfig.setEnable(true);
            extraFileConfig.setName(s);
            extraFileConfig.setOutputPath(BaseConstants.MG_EXAMPLE_HOME + "mybatis-friend-test/com/test/usual");
            extraFileConfig.setPackageName("com.test");
            return extraFileConfig;
        }).collect(Collectors.toSet());
        extraFileGroupConfig1.setExtraFileConfigNames(extraFileConfigSet1);
        extraFileGroupConfigs.add(extraFileGroupConfig1);

        ExtraFileGroupConfig extraFileGroupConfig = new ExtraFileGroupConfig();
        extraFileGroupConfig.setEnable(false);
        extraFileGroupConfig.setIsSystem(true);
        extraFileGroupConfig.setGroupName("cola 架构 test");
        final Set<ExtraFileGroupConfig.ExtraFileConfig> extraFileConfigSet = Stream.of("AddCmdExe", "ByIdQryExe", "Controller", "DelByIdCmdExe", "DOConvertMapper",
                "GatewayI", "GatewayImpl", "PageQryExe", "ServiceI", "ServiceImpl", "UpdateCmdExe").map(s -> {
            ExtraFileGroupConfig.ExtraFileConfig extraFileConfig = new ExtraFileGroupConfig.ExtraFileConfig();
            extraFileConfig.setEnable(true);
            extraFileConfig.setName(s);
            extraFileConfig.setOutputPath(BaseConstants.MG_EXAMPLE_HOME + "mybatis-friend-test/com/test/cola");
            extraFileConfig.setPackageName("com.test");
            return extraFileConfig;
        }).collect(Collectors.toSet());
        extraFileGroupConfig.setExtraFileConfigNames(extraFileConfigSet);
        extraFileGroupConfigs.add(extraFileGroupConfig);

        return extraFileGroupConfigs;
    }
}
