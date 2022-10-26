package com.alan344.service;

import com.alan344.bean.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.BeanUtils;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONWriter;
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
     * @param mybatisExportConfig 配置信息
     * @return 1:原来文件被修改；2：已存在配置且和原来配置相同；3：新配置
     */
    public int addConfig(MybatisExportConfig mybatisExportConfig) {
        File configFile = BaseConstants.getConfigFile();
        LinkedList<MybatisExportConfig> mybatisExportConfigs;
        if (configFile.exists()) {
            mybatisExportConfigs = this.loadConfigFromFile();
        } else {
            mybatisExportConfigs = new LinkedList<>();
        }

        LinkedList<MybatisExportConfig> existConfigLinkedList = mybatisExportConfigs.stream().filter(generatorConfig1 -> mybatisExportConfig.getConfigName().equals(generatorConfig1.getConfigName()))
                .collect(LinkedList::new, LinkedList::add, List::addAll);

        //配置已存在，如果内容修改，则修改
        if (!existConfigLinkedList.isEmpty()) {
            MybatisExportConfig olderConfig = existConfigLinkedList.getFirst();
            boolean isSame = BeanUtils.checkPropertyOfBean(mybatisExportConfig, olderConfig);
            if (!isSame) {
                mybatisExportConfigs.remove(olderConfig);
                mybatisExportConfigs.addFirst(mybatisExportConfig);
                this.downLoadConfigToFile(mybatisExportConfigs);
                return 1;
            } else {
                return 2;
            }
        } else {
            mybatisExportConfigs.addFirst(mybatisExportConfig);
            this.downLoadConfigToFile(mybatisExportConfigs);
            return 3;
        }
    }

    /**
     * 删除配置
     *
     * @param mybatisExportConfig 配置信息
     */
    public void deleteConfig(MybatisExportConfig mybatisExportConfig) {
        List<MybatisExportConfig> mybatisExportConfigs = loadConfigFromFile();
        mybatisExportConfigs.remove(mybatisExportConfig);
        this.downLoadConfigToFile(mybatisExportConfigs);
    }

    /**
     * 把配置写入文件
     *
     * @param mybatisExportConfigs 配置信息
     */
    private void downLoadConfigToFile(List<MybatisExportConfig> mybatisExportConfigs) {
        String configsStr = JSONArray.toJSONString(mybatisExportConfigs, JSONWriter.Feature.PrettyFormat);
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
            return new LinkedList<>();
        }

        try {
            List<MybatisExportConfig> mybatisExportConfigs = JSONArray.parseArray(FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString())).toList(MybatisExportConfig.class);
            return mybatisExportConfigs.stream().collect(LinkedList::new, LinkedList::add, List::addAll);
        } catch (IOException e) {
            log.error("加载dataSource文件失败", e);
            return new LinkedList<>();
        }
    }
}
