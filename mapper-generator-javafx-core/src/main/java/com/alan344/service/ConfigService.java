package com.alan344.service;

import com.alan344.bean.GeneratorConfig;
import com.alan344.constants.BaseConstants;
import com.alan344happyframework.util.BeanUtils;
import com.alibaba.fastjson.JSONArray;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.LinkedList;
import java.util.List;

/**
 * @author AlanSun
 * @date 2019/8/15 18:10
 */
@Slf4j
@Service
public class ConfigService {
    /**
     * 添加配置
     *
     * @param generatorConfig 配置信息
     * @return 1:原来文件被修改；2：已存在配置且和原来配置相同；3：新配置
     */
    public int addConfig(GeneratorConfig generatorConfig) {
        File configFile = BaseConstants.getConfigFile();
        LinkedList<GeneratorConfig> generatorConfigs;
        if (configFile.exists()) {
            generatorConfigs = this.loadConfigFromFile();
        } else {
            generatorConfigs = new LinkedList<>();
        }

        LinkedList<GeneratorConfig> existConfigLinkedList = generatorConfigs.stream().filter(generatorConfig1 -> generatorConfig.getConfigName().equals(generatorConfig1.getConfigName())).collect(Lists::newLinkedList, LinkedList::add, List::addAll);

        //配置已存在，如果内容修改，则修改
        if (!existConfigLinkedList.isEmpty()) {
            GeneratorConfig olderConfig = existConfigLinkedList.getFirst();
            boolean isSame = BeanUtils.checkPropertyOfBean(generatorConfig, olderConfig);
            if (!isSame) {
                existConfigLinkedList.remove(olderConfig);
                existConfigLinkedList.addFirst(generatorConfig);
                this.downLoadConfigToFile(existConfigLinkedList);
                return 1;
            } else {
                return 2;
            }
        } else {
            generatorConfigs.addFirst(generatorConfig);
            this.downLoadConfigToFile(generatorConfigs);
            return 3;
        }
    }

    /**
     * 删除配置
     *
     * @param generatorConfig 配置信息
     */
    public void deleteConfig(GeneratorConfig generatorConfig) {
        List<GeneratorConfig> generatorConfigs = loadConfigFromFile();
        generatorConfigs.remove(generatorConfig);
        this.downLoadConfigToFile(generatorConfigs);
    }

    /**
     * 把配置写入文件
     *
     * @param generatorConfigs 配置信息
     */
    private void downLoadConfigToFile(List<GeneratorConfig> generatorConfigs) {
        String configsStr = JSONArray.toJSONString(generatorConfigs, true);
        try {
            FileUtils.writeStringToFile(BaseConstants.getConfigFile(), configsStr, StandardCharsets.UTF_8.toString());
        } catch (IOException e) {
            log.error("写入配置信息失败", e);
        }
    }


    /**
     * 从文件加载配置至pane
     */
    public LinkedList<GeneratorConfig> loadConfigFromFile() {
        File file = BaseConstants.getConfigFile();
        if (!file.exists()) {
            return Lists.newLinkedList();
        }

        try {
            String s = FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString());
            log.info(s);
            List<GeneratorConfig> generatorConfigs = JSONArray.parseArray(FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString()), GeneratorConfig.class);
            return generatorConfigs.stream().collect(Lists::newLinkedList, LinkedList::add, List::addAll);
        } catch (IOException e) {
            log.error("加载dataSource文件失败", e);
            return Lists.newLinkedList();
        }
    }
}
