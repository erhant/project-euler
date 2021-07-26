/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */

int getDecimalValue(struct ListNode* head) {
  int ans = 0;
  ans = head->val;
  while (head->next) {
    head = head->next;
    ans = (ans << 1) | head->val;  // e.g. 01 and 1 -> 010 & 1 = 011
  }
  return ans;
}