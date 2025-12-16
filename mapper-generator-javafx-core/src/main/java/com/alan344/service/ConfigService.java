package com.alan344.service;

import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.ExtraFileGroupConfig;
import com.alan344.bean.config.ExtraTemplateFileConfig;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.CollectionUtils;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONWriter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author AlanSun
 * @since 2019/8/15 18:10
 */
@Slf4j
@Service
public class ConfigService {
    @Value(value = "classpath:default-extra-file-config.json")
    private Resource resource;

    @Autowired
    private ExtraTemplateFileConfigService extraTemplateFileConfigService;

    private LinkedList<MybatisExportConfig> mybatisExportConfigs2 = new LinkedList<>();

    private boolean isLoaded;

    /**
     * 把配置写入文件
     */
    void saveConfigToFile() {
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
            mybatisExportConfig.setExtraFileGroupConfigs(defaults);
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
            final List<String> templateIds = extraFileGroupConfigs.stream().flatMap(extraFileGroupConfig -> extraFileGroupConfig.getExtraFileConfigs().stream().map(ExtraFileGroupConfig.ExtraFileConfig::getTemplateId)).toList();
            final Map<String, ExtraTemplateFileConfig> templateIdExtraFileConfigMap = extraTemplateFileConfigService.getExtraFileConfigMap(templateIds);

            final String sameFromPackage = this.getSameFromPackage(mybatisExportConfig);

            // 设置内置示例的导出地址
            extraFileGroupConfigs.forEach(extraFileGroupConfig -> {
                final Collection<ExtraFileGroupConfig.ExtraFileConfig> list = extraFileGroupConfig.getList();
                list.forEach(extraFileConfig -> {
                    final ExtraTemplateFileConfig extraTemplateFileConfig = templateIdExtraFileConfigMap.get(extraFileConfig.getTemplateId());
                    this.fillOutputPathAndPackageName(extraFileConfig, extraTemplateFileConfig, mybatisExportConfig, sameFromPackage);
                });
            });
            return extraFileGroupConfigs;
        } catch (IOException e) {
            log.error("获取默认分组失败", e);
        }

        return Collections.emptyList();
    }

    public String getSameFromPackage(MybatisExportConfig mybatisExportConfig) {
        final List<String> split1 = StrUtil.split(mybatisExportConfig.getBeanPackage(), ".");
        final List<String> split2 = StrUtil.split(mybatisExportConfig.getMapperPackage(), ".");

        if (split1.size() == 0 || split2.size() == 0) {
            return "";
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < split1.size(); i++) {
            if (split1.get(i).equals(split2.get(i))) {
                sb.append(split1.get(i)).append(".");
            } else {
                break;
            }
        }

        return StrUtil.removeSuffix(sb.toString(), ".");
    }

    /**
     * 填充输出路径和包名
     */
    public void fillOutputPathAndPackageName(ExtraFileGroupConfig.ExtraFileConfig extraFileConfig, ExtraTemplateFileConfig extraTemplateFileConfig, MybatisExportConfig mybatisExportConfig, String sameFromPackage) {
        // 填充默认值
        // 如果 extraTemplateFileConfig.getDefaultOutputPathSuffix() 为空则使用 mybatisExportConfig.getBeanLocation()
        // 如果 extraTemplateFileConfig.getDefaultOutputPathSuffix() 以 - 开头则使用 mybatisExportConfig.getProjectName() + extraTemplateFileConfig.getDefaultOutputPathSuffix() + mybatisExportConfig.getBeanLocation()
        // 否则 使用 extraTemplateFileConfig.getDefaultOutputPathSuffix() + mybatisExportConfig.getBeanLocation()
        extraFileConfig.setOutputPath(StrUtil.isEmpty(extraTemplateFileConfig.getDefaultOutputPathSuffix()) ? mybatisExportConfig.getBeanLocation() :
                extraTemplateFileConfig.getDefaultOutputPathSuffix().startsWith("-") ?
                        StrUtil.removePrefix(StrUtil.addSuffixIfNot(mybatisExportConfig.getProjectName() + extraTemplateFileConfig.getDefaultOutputPathSuffix(), StrUtil.SLASH) + mybatisExportConfig.getBeanLocation(), StrUtil.SLASH) :
                        StrUtil.removePrefix(StrUtil.addSuffixIfNot(extraTemplateFileConfig.getDefaultOutputPathSuffix(), StrUtil.SLASH) + mybatisExportConfig.getBeanLocation(), StrUtil.SLASH));

        // 如果 sameFromPackage 为空 判断 extraTemplateFileConfig.getDefaultPackageSuffix() 是否为空 如果为空则使用空字符串 否则使用 extraTemplateFileConfig.getDefaultPackageSuffix()
        // 如果 sameFromPackage 不为空 判断 extraTemplateFileConfig.getDefaultPackageSuffix() 是否为空 如果为空则使用 sameFromPackage 否则使用 sameFromPackage + extraTemplateFileConfig.getDefaultPackageSuffix()
        extraFileConfig.setPackageName(StrUtil.isEmpty(sameFromPackage) ?
                (StrUtil.isEmpty(extraTemplateFileConfig.getDefaultPackageSuffix()) ? "" : extraTemplateFileConfig.getDefaultPackageSuffix()) :
                (StrUtil.isEmpty(extraTemplateFileConfig.getDefaultPackageSuffix()) ? sameFromPackage :
                        StrUtil.addSuffixIfNot(sameFromPackage, ".") + StrUtil.removeSuffix(StrUtil.removePrefix(extraTemplateFileConfig.getDefaultPackageSuffix(), "."), ".")));
    }
}
