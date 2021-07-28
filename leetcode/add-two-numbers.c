/*
topics:
  - linked list
  - math
  - hash table
difficulty: medium
*/

/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */

struct ListNode* addTwoNumbers(struct ListNode* l1, struct ListNode* l2) {
  struct ListNode* ans = (struct ListNode*)malloc(sizeof(struct ListNode));
  struct ListNode* head;
  bool carry = false;
  head = ans;
  ans->val = l1->val + l2->val;
  ans->next = NULL;
  if (ans->val >= 10) {
    carry = true;
    ans->val = ans->val - 10;
  }
  // printf("%d (%d + %d)\n",ans->val, l1->val, l2->val);
  while (l1->next != NULL && l2->next != NULL) {
    ans->next = (struct ListNode*)malloc(sizeof(struct ListNode));
    l1 = l1->next;
    l2 = l2->next;
    ans = ans->next;
    ans->val = l1->val + l2->val + carry;
    ans->next = NULL;
    carry = false;
    if (ans->val >= 10) {
      carry = true;
      ans->val = ans->val - 10;
    }
    // printf("%d (%d + %d)\n",ans->val, l1->val, l2->val);
  }

  // only one of them will run
  while (l1->next != NULL) {
    ans->next = (struct ListNode*)malloc(sizeof(struct ListNode));
    l1 = l1->next;
    ans = ans->next;
    ans->val = l1->val + carry;
    ans->next = NULL;
    carry = false;
    if (ans->val >= 10) {
      carry = true;
      ans->val = ans->val - 10;
    }
    // printf("%d (%d)\n",ans->val, l1->val);
  }
  while (l2->next != NULL) {
    ans->next = (struct ListNode*)malloc(sizeof(struct ListNode));
    l2 = l2->next;
    ans = ans->next;
    ans->val = l2->val + carry;
    ans->next = NULL;
    carry = false;
    if (ans->val >= 10) {
      carry = true;
      ans->val = ans->val - 10;
    }
    // printf("%d (%d)\n",ans->val, l2->val);
  }
  if (carry) {
    ans->next = (struct ListNode*)malloc(sizeof(struct ListNode));
    ans->next->val = 1;
    ans->next->next = NULL;
    // printf("1 (C)\n");
  }
  return head;
}