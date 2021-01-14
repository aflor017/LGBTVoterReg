library(tidyverse)
library(ggthemes)

my_df <- as_tibble(list(est = c(.7401351, .7100185, .7782334, .7999483, .7568356, .6178908, .6768085, .6261563, .728507, .5744534, .5221626,                    
                                .7324571, .7073546, .7807265, .8102904, .8010797, .7410851, .7532785, .6869054, .7256129, .6855569, .6090239,
                                .8542467, .8075256, .851259, .8820054, .8552543, .7637903, .8200567, .7545528, .8295119, .7361183, .685169,
                                .835228, .8165202, .8587684, .8781318, .8702149, .8403042, .8387609, .8024983, .8280469, .8007045, .7543186,
                                .8425549, .7917188, .8172868, .8887153, .8178207, .730384, .8242257, .7404676, .8171723, .6966603, .5458289,
                                .817626, .8044734, .8429982, .8742549, .8482972, .8315117, .8200728, .8085766, .8160931, .7838952, .6798854),
                        lb = c(.7272521, .7001053, .7231906, .7548702, .6866854, .5651467, .5946353, .5645751, .7208525, .4992121, .4428408,
                              .7203404, .6983845, .7298843, .7703712, .7474478, .7003865, .7020695, .629448, .7186813, .6229569, .5384545,
                              .8487718, .8023236, .8214156, .8641572, .8248775, .7363904, .7867038, .7228194, .825902, .6985839, .6424712,
                              .8293643, .8117085, .8313271, .8604288, .8433414, .8205318, .8087748, .7764668, .8245698, .7708361, .719013,
                              .8373154, .7867272, .7885876, .8725656, .7862366, .7080274, .7955621, .7123682, .8137342, .6642581, .5059002,
                              .8119514, .7999526, .8187125, .8571924, .8220189, .8162476, .7918852, .7868016, .8128095, .759024, .6465533),
                        ub = c(.7530181, .7199317, .8332762, .8450265, .8269858, .6706349, .7589817, .6877376, .7361616, .6496947, .6014845,
                               .7445739, .7163246, .8315687, .8502097, .8547116, .7817836, .8044874, .7443629, .7325445, .7481568, .6795933,
                               .8597216, .8127276, .8811024, .8998537, .8856311, .7911902, .8534096, .7862861, .8331218, .7736528, .7278668,
                               .8410917, .8213319, .8862098, .8958348, .8970884, .8600766, .868747, .8285297, .831524, .8305729, .7896242,
                               .8477944, .7967103, .8459861, .9048651, .8494047, .7527406, .8528893, .768567, .8206104, .7290626, .5857576,
                               .8233006, .8089941, .8672838, .8913174, .8745756, .8467758, .8482605, .8303516, .8193766, .8087665, .7132175),
                        year = c(rep("2016 (commonweight)", 22), rep("2016 (unweighted)", 22), rep("2018 (unweighted)",22)),
                        gr = rep(c("Straight Men", "Straight Women", "Lesbians", "Gay Men", "Bisexual Men", "Bisexual Women", "Other Men","Other Women","Cisgender","Transgender", "Other gender expression"), 6),
                        mod = c(rep("Unadjusted",11), rep("Adjusted",11), rep("Unadjusted",11), rep("Adjusted", 11),  rep("Unadjusted",11), rep("Adjusted", 11))))

my_df <- my_df %>%
  mutate(gr = factor(gr, levels = c("Straight Men", "Gay Men", "Bisexual Men", "Other Men", "Straight Women", "Lesbians", "Bisexual Women", "Other Women", "Cisgender", "Transgender", "Other gender expression")))

dummy2 <- data.frame(year = c("2016 (commonweight)", "2016 (unweighted)", "2018 (unweighted)"), Z = c(.7324571, .835228, .817626))         


p <- ggplot(my_df, aes(x = forcats::fct_rev(gr), y=est, ymin = lb, ymax = ub, color = mod, fill = mod))

p + geom_hline(data = dummy2, aes(yintercept = Z), linetype = 2, color = "red") + 
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.8) +
  theme_clean() +
  scale_color_manual(values = c("black","gray")) + 
  theme(legend.background = element_blank()) +
  labs(x = NULL,
       y = "Pr(Registered to Vote)",
       fill = NULL,
       color = NULL) +
  coord_flip() +
  facet_wrap(~year, ncol = 3) 

ggsave("poq_aux.tiff", device = "tiff", path = "C:/Users/aflor/Downloads/", dpi=300)

### Appendix figure; repeat code but 3 sets of preds
my_df <- as_tibble(list(est = c(.8183131, .8047086, .8524778, .8591512, .8109169, .7517381, .7708361, .7291609, .8140454, .6951804, .6583979,                    
                                .8160918, .8001426, .8537732, .867943, .8464709, .8282257, .8312902, .7713292, .8126294, .767699, .7139143,
                                .7704024, .7762964, .7997203, .8307924, .7429755, .6808566, .7039713, .6875474, .7784608, .5791188, .5059077,
                                .7705189, .7658418, .8246705, .8374033, .8111409, .7855915, .7436941, .7424908, .7749578, .7066371, .6515211),
                        lb = c(.8080832, .7973248, .8168582, .8270412, .7595586, .713096, .6937049, .6781582, .80812, .6290105, .591527,
                               .8062143, .7929718, .8185452, .8389119, .8041697, .7997613, .780435, .7251906, .8069959, .7161604, .6587286,
                               .760611, .7688716, .7542748, .7977706, .6900023, .6441427, .6417957, .6371112, .7726659, .5237255, .4436526,
                               .7613039, .7586229, .7888446, .8095052, .7718566, .7609534, .6879446, .697603, .7694612, .6620786, .5999248),
                        ub = c(.828543, .8120924, .8880973, .8912612, .8622753, .7903801, .8479673, .7801637, .8199708, .7613503, .7252688,
                               .8259693, .8073133, .8890011, .8969742, .888772, .8566901, .8821455, .8174677, .8182629, .8192376, .7690999,
                               .7801937, .7837213, .8451657, .8638142, .7959487, .7175706, .766147, .7379835, .7842557, .6345122, .5681628,
                               .779734, .7730606, .8604964, .8653015, .8504252, .8102296, .7994436, .7873786, .7804544, .7511956, .7031173),
                        year = c(rep(2016, 22), rep(2018,22)),
                        gr = rep(c("Straight Men", "Straight Women", "Lesbians", "Gay Men", "Bisexual Men", "Bisexual Women", "Other Men","Other Women","Cisgender","Transgender", "Other gender expression"), 4),
                        mod = c(rep("Unadjusted",11), rep("Adjusted",11), rep("Unadjusted",11), rep("Adjusted", 11))))

my_df <- my_df %>%
  mutate(gr = factor(gr, levels = c("Straight Men", "Gay Men", "Bisexual Men", "Other Men", "Straight Women", "Lesbians", "Bisexual Women", "Other Women", "Cisgender", "Transgender", "Other gender expression")))

dummy2 <- data.frame(year = c(2016, 2018), Z = c(.8160918, .7705189))         


p <- ggplot(my_df, aes(x = forcats::fct_rev(gr), y=est, ymin = lb, ymax = ub, color = mod, fill = mod))

p + geom_hline(data = dummy2, aes(yintercept = Z), linetype = 2, color = "red") + 
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.8) +
  theme_clean() +
  scale_color_manual(values = c("black","gray")) + 
  theme(legend.background = element_blank()) +
  labs(x = NULL,
       y = "Pr(Registered to Vote)",
       fill = NULL,
       color = NULL) +
  coord_flip() +
  facet_wrap(~year, ncol = 3) 

ggsave("poq_app.tiff", device = "tiff", path = "C:/Users/aflor/Downloads/", dpi = 300)