package com.alan344.bean.config;

import com.alan344.component.LeftRightLinkageBorderPane;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.annotation.JSONField;
import lombok.Getter;
import lombok.Setter;

import java.util.Collection;
import java.util.UUID;

/**
 * @author AlanSun
 * @since 2023/1/30 15:58
 */
@Getter
@Setter
public class ExtraTemplateFileGroupConfig implements LeftRightLinkageBorderPane.GroupName, Cloneable {
    /**
     * 分组名称
     */
    private String groupName;

    private boolean isEnable;
    /**
     * 是否是系统内置的配置
     */
    private boolean isSystem;

    /**
     * 标识该配置是否已经保存到磁盘
     */
    private boolean isSaved = true;

    private Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList;

    @Override
    public ExtraTemplateFileGroupConfig clone() {
        final ExtraTemplateFileGroupConfig groupConfig =
                JSON.parseObject(JSON.toJSONString(this), ExtraTemplateFileGroupConfig.class);
        groupConfig.setSaved(false);
        final Collection<ExtraTemplateFileConfig> configList = groupConfig.getExtraTemplateFileConfigList();
        configList.forEach(extraTemplateFileConfig -> {
            extraTemplateFileConfig.setId(UUID.randomUUID().toString());
            extraTemplateFileConfig.setExtraTemplateFileGroupConfig(groupConfig);
        });
        return groupConfig;
    }

    @JSONField(serialize = false, deserialize = false)
    @Override
    public Collection<ExtraTemplateFileConfig> getList() {
        return this.extraTemplateFileConfigList;
    }

    @JSONField(serialize = false, deserialize = false)
    @Override
    public void setList(Collection list) {
        this.extraTemplateFileConfigList = list;
    }
}
