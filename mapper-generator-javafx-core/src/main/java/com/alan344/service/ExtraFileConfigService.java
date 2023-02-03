package com.alan344.service;

import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.ExtraTemplateFileGroupConfig;
import com.alan344.constants.BaseConstants;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONWriter;
import org.apache.commons.io.FileUtils;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author AlanSun
 * @date 2022/11/19 23:35
 */
@Service
public class ExtraFileConfigService {

    private List<ExtraTemplateFileGroupConfig> extraTemplateFileConfigs;

    public void saveExtraFileConfig(List<ExtraTemplateFileGroupConfig> items) {
        extraTemplateFileConfigs = items;
        try {
            FileUtils.writeStringToFile(BaseConstants.getExtraFileConfigFile(), JSONArray.toJSONString(this.getExtraFileConfigList(), JSONWriter.Feature.PrettyFormat, JSONWriter.Feature.WriteEnumsUsingName), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public Map<String, ExtraTemplateFileConfig> getExtraFileConfigMap(List<String> groupNameNameList) {
        final List<ExtraTemplateFileGroupConfig> extraTemplateFileConfigList = this.getExtraFileConfigList();
        Map<String, ExtraTemplateFileConfig> stringExtraTemplateFileConfigMap = new HashMap<>(16);
        for (ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig : extraTemplateFileConfigList) {
            final List<ExtraTemplateFileConfig> extraTemplateFileConfigList1 = extraTemplateFileGroupConfig.getExtraTemplateFileConfigList();
            for (ExtraTemplateFileConfig extraTemplateFileConfig : extraTemplateFileConfigList1) {
                stringExtraTemplateFileConfigMap.put(extraTemplateFileGroupConfig.getGroupName() + ":" + extraTemplateFileConfig.getName(), extraTemplateFileConfig);
            }
        }

        return stringExtraTemplateFileConfigMap;
    }

    public List<ExtraTemplateFileGroupConfig> getExtraFileConfigList() {
        if (null != extraTemplateFileConfigs) {
            return extraTemplateFileConfigs;
        }

        final File extraFileConfigFile = BaseConstants.getExtraFileConfigFile();
        if (!extraFileConfigFile.exists()) {
            extraTemplateFileConfigs = new ArrayList<>();
        } else {
            try {
                extraTemplateFileConfigs = JSONArray.parseArray(FileUtils.readFileToString(extraFileConfigFile, StandardCharsets.UTF_8)).toList(ExtraTemplateFileGroupConfig.class);
            } catch (IOException e) {
                throw new RuntimeException(e);
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
        return Collections.emptyList();
    }
}
